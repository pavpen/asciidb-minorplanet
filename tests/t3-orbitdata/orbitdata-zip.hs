{-# LANGUAGE BangPatterns, GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    NamedFieldPuns, OverloadedStrings, QuasiQuotes, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Monad			(forM_)
import Control.Monad.IO.Class		(liftIO)
import qualified Data.ASCII.LowellObservatory.AstOrb as AstOrb
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Binary			(encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.List			(isSuffixOf)
import qualified Data.List as L
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime,
					 diffUTCTime)
import Data.Time.Clock.POSIX		(utcTimeToPOSIXSeconds)
import Data.Time.Calendar		(fromGregorian)
import Database.Groundhog		(PersistEntity (..), Order (..), (==.),
					 (&&.), (||.), runMigration,
					 defaultMigrationLogger, get, insert,
					 select, selectAll, orderBy)
import Database.Groundhog.Sqlite	(runSqliteConn, withSqliteConn)
import qualified Database.Persist.MinorPlanetCenter as MPCDb
import System.Environment		(getProgName, getArgs)
import System.IO			(stderr, hPutStrLn)
import Text.Printf			(printf)


domineT = UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)
epochT = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime $ 12 * 3600)

readDecomprFile path = do
	bs <- LBS.readFile path
	return $ decompr bs
  where decompr bs | ".gz" `isSuffixOf` path = decompress bs
  		   | otherwise		     = bs

useRecs (AstOrb.Rec {..}) (MPCObs.Rec {time}) = do
	putStrLn $
	  printf "{ %16.8f, %16.8f, %16.8f, %16.8f, %16.8f, %16.8f, %s },"
	  	 semiMajAAU ecc
		 (inclRad * 180 / pi)
		 (lngAscNodeRad * 180 / pi)
		 (argPerihRad * 180 / pi)
		 (meanAnomRad * 180 / pi)
		 (show $ round $ time `diffUTCTime` epochT)

filterTObs recs = doFilter domineT recs
  where doFilter :: UTCTime -> [MPCObs.Rec] -> [MPCObs.Rec]
  	doFilter _ [] = []
  	doFilter lastT (lm:restLms)
	  | lmT == lastT = doFilter lastT restLms
	  | otherwise	 = lm:(doFilter lmT restLms)
	  where lmT = MPCObs.time lm

filterSortedObs recs = doFilter (MPCObs.DesignNum (-1)) recs
  where doFilter :: MPCObs.Design -> [MPCObs.Rec] -> [MPCObs.Rec]
  	doFilter _ [] = []
  	doFilter lastDesign (lm:restLms)
	  | lmDesign == lastDesign = doFilter lastDesign restLms
	  | otherwise		   = lm:(doFilter lmDesign restLms)
	  where lmDesign = MPCObs.getDesign lm

filterObs (rec:restRecs) = doFilter rec restRecs
  where doFilter :: MPCObs.Rec -> [MPCObs.Rec] -> [MPCObs.Rec]
  	doFilter lastLm [] = [lastLm]
  	doFilter lastLm (lm:restLms)
	  | lmDesign == lastDesign = doFilter (chooseLm lastLm lm) restLms
	  | otherwise		   = lastLm:(doFilter lm restLms)
	  where lmDesign = MPCObs.getDesign lm
	  	lastDesign = MPCObs.getDesign lastLm
	chooseLm lm1@(MPCObs.Rec {time=t1}) lm2@(MPCObs.Rec {time=t2})
	  | t1 <= t2  = lm1
	  | otherwise = lm2
filterObs [] = []

filterObs' recs = map reduceGrp grps
  where grps = L.groupBy (\a b -> MPCObs.getDesign a == MPCObs.getDesign b) recs
  	reduceGrp = foldr1 chooseLm
	chooseLm lm1@(MPCObs.Rec {time=t1}) lm2@(MPCObs.Rec {time=t2})
	  | t1 <= t2  = lm1
	  | otherwise = lm2

main = do
	progName <- getProgName
	putStrLn $ "Hi!  " ++ progName ++ " here!"
	args <- getArgs
	let (astOrbFPath, numObsFPath) = case args of
		(path1:path2:_) -> (path1, path2)
		[path]   -> (path, "../t1-read/NumObs.txt.gz")
		_   -> ("../t1-read/astorb.dat.gz", "../t1-read/NumObs.txt.gz")
	astOrbBs <- readDecomprFile astOrbFPath
	obsBs <- readDecomprFile numObsFPath
	let astOrbRecs = AstOrb.getRecs astOrbBs
	let obsRecs = filterObs' $ MPCObs.getRecs obsBs
	hPutStrLn stderr $ "Zipping astOrbRecs & obsRecs."
	mapM_ procsRec $ zip astOrbRecs obsRecs
  where procsRec (astOrbRec, mpcObsRec)
  	  | MPCObs.getDesign astOrbRec == MPCObs.getDesign mpcObsRec = do
	    -- hPutStrLn stderr $ (show astOrbRec)
  	    useRecs astOrbRec mpcObsRec
	  | otherwise =
	    hPutStrLn stderr $ "Records with unmatching IDs:\n" ++
	    	(LCh8.unpack $ encode astOrbRec) ++
		(LCh8.unpack $ encode mpcObsRec)
