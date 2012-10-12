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
import System.Environment		(getProgName, getArgs)
import System.IO			(stderr, hPutStr, hPutStrLn)
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

procsRecs rNum (orbRec:restOrbRecs) (obsRec:restObsRecs) = do
	hPutStr stderr $ (replicate 40 '\b') ++ "Record: " ++ (show rNum)
	useRecs orbRec obsRec
	procsRecs (rNum + 1) restOrbRecs restObsRecs
procsRecs _ [] [] = do
	hPutStrLn stderr $ "\nDone."
procsRecs _ [] _ = do
	hPutStrLn stderr $ "\nRan out of AstOrb records."
procsRecs _ _ [] = do
	hPutStrLn stderr $ "\nRan out of MPC observation records."

main = do
	progName <- getProgName
	putStrLn $ "Hi!  " ++ progName ++ " here!"
	args <- getArgs
	let (astOrbFPath, numObsFPath) = case args of
		(path1:path2:_) -> (path1, path2)
		[path]   -> (path, "../t1-read/NumObs.txt.gz")
		_   -> ("../t1-read/astorb.dat.gz", "outp/filtered-obs.txt")
	astOrbBs <- readDecomprFile astOrbFPath
	obsBs <- readDecomprFile numObsFPath
	let astOrbRecs = AstOrb.getRecs astOrbBs
	let obsRecs = MPCObs.getRecs obsBs
	hPutStrLn stderr $ "Zipping astOrbRecs & obsRecs."
	-- mapM_ procsRec $ zip astOrbRecs obsRecs
	procsRecs 1 astOrbRecs obsRecs
  where procsRec (astOrbRec, mpcObsRec)
  	  | MPCObs.getDesign astOrbRec == MPCObs.getDesign mpcObsRec = do
	    -- hPutStrLn stderr $ (show astOrbRec)
  	    useRecs astOrbRec mpcObsRec
	  | otherwise =
	    hPutStrLn stderr $ "Records with unmatching IDs:\n" ++
	    	(LCh8.unpack $ encode astOrbRec) ++
		(LCh8.unpack $ encode mpcObsRec)
