{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
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
	  printf "{ %16.8f, %16.8f, %16.8f, %16.8f, %16.8f, %16.8f, %s }"
	  	 semiMajAAU ecc
		 (inclRad * 180 / pi)
		 (lngAscNodeRad * 180 / pi)
		 (argPerihRad * 180 / pi)
		 (meanAnomRad * 180 / pi)
		 (show $ round $ time `diffUTCTime` epochT)

filterNumObs recs = doFilter (MPCObs.DesignNum (-1)) recs
  where doFilter :: MPCObs.Design -> [MPCObs.Rec] -> [MPCObs.Rec]
  	doFilter _ [] = []
  	doFilter lastDesign (lm:restLms)
	  | lmDesign == lastDesign = doFilter lastDesign restLms
	  | otherwise		   = lm:(doFilter lmDesign restLms)
	  where lmDesign = MPCObs.getDesign lm

main = do
	progName <- getProgName
	putStrLn $ "Hi!  " ++ progName ++ " here!"
	args <- getArgs
	let (astOrbFPath, numObsFPath) = case args of
		(path1:path2:_) -> (path1, path2)
		[path]   -> (path, "../t1-read/NumObs.txt.gz")
		_   -> ("../t1-read/astorb.dat.gz", "../t1-read/NumObs.txt.gz")
	astOrbBs <- readDecomprFile astOrbFPath
	numObsBs <- readDecomprFile numObsFPath
	let astOrbRecs = AstOrb.getRecs astOrbBs
	let numObsRecs = filterNumObs $ MPCObs.getRecs numObsBs
	{-
	withSqliteConn "../t2-persist/db/mpcobs-filtered-1.sqlite" $
	 runSqliteConn $ do
	  runMigration defaultMigrationLogger MPCDb.migrateAll
	  -- mapM_ procsRec' inpRecs
	  liftIO $ putStrLn $ "Reading numObsRecs."
	  dbRecs <- selectAll
	  liftIO $ putStrLn $ "Zipping astOrbRecs & numObsRecs."
	  let numObsRecs = map (MPCDb.fromDbRec . snd) dbRecs
	  liftIO $ mapM_ procsRec $ zip astOrbRecs $ numObsRecs
	  -- return $ map (MPCDb.fromDbRec . snd) dbRecs
	-}
	hPutStrLn stderr $ "Zipping astOrbRecs & numObsRecs."
	mapM_ procsRec $ zip astOrbRecs numObsRecs
  where procsRec (astOrbRec, mpcObsRec)
  	  | MPCObs.getDesign astOrbRec == MPCObs.getDesign mpcObsRec = do
	    -- hPutStrLn stderr $ (show astOrbRec)
  	    useRecs astOrbRec mpcObsRec
	  | otherwise =
	    hPutStrLn stderr $ "Records with unmatching IDs:\n" ++
	    	(LCh8.unpack $ encode astOrbRec) ++
		(LCh8.unpack $ encode mpcObsRec)
  	procsRec' rec@(AstOrb.Rec {..}) = do
	  -- liftIO $ LCh8.putStrLn $ encode rec
  	  obs@(MPCDb.DbRec { time }) <- getDiscovObs rec
	  -- liftIO $ print obs
	  liftIO $ putStrLn $ "{ " ++
	  	(show semiMajAAU) ++ ", " ++
	  	(show ecc) ++ ", " ++
		(show $ inclRad * 180 / pi) ++ ", " ++
		(show $ lngAscNodeRad * 180 / pi) ++ ", " ++
		(show $ argPerihRad * 180 / pi) ++ ", " ++
		(show $ meanAnomRad * 180 / pi) ++ ", " ++
		(show $ round $ time `diffUTCTime` epochT) ++ " },"
	getDiscovObs (AstOrb.Rec {..})
	  | objNumber == -1 =
	    do let pd = MPCDb.toDbProvDesign $ PD.readLong designation
	       -- liftIO $ putStrLn $ "getDiscovObs: objNumber is -1, pd: " ++
	       --				(show pd)
	       recs <- select $ (MPCDb.DbRecDbProvDesign ==. Just pd) `orderBy`
				[Desc MPCDb.DbRecTime]
	       -- liftIO $ putStrLn $ "Discovery recs: "
	       -- liftIO $ mapM_ (print . encode . MPCDb.fromDbRec) recs
	       return $ head recs
	  | otherwise =
	    do -- liftIO $ putStrLn $ "getDiscovObs: objNumber is " ++
	       --				(show objNumber)
	       recs <- select $ (MPCDb.DbRecObjNumber ==. objNumber) `orderBy`
				[Desc MPCDb.DbRecTime]
	       -- liftIO $ putStrLn $ "Discovery recs: "
	       -- liftIO $ mapM_ (print . encode . MPCDb.fromDbRec) recs
	       return $ head recs
