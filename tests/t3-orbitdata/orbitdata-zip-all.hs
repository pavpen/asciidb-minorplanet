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
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime,
					 diffUTCTime)
import Data.Time.Clock.POSIX		(utcTimeToPOSIXSeconds)
import Data.Time.Calendar		(fromGregorian)
import System.Environment		(getProgName, getArgs)
import System.IO			(stderr, hPutStrLn, hPutStr)
import Text.Printf			(printf)


domineT = UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)
j2000T = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime $ 12 * 3600)

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
		 (show $ round $ time `diffUTCTime` osculationEpochT)
  where osculationEpochT = UTCTime osculationEpoch (secondsToDiffTime 0)

filterObs !(rec:restRecs) = doFilter (MPCObs.getDesign rec, rec) restRecs
  where doFilter :: (MPCObs.Design, MPCObs.Rec) -> [MPCObs.Rec] -> [MPCObs.Rec]
  	doFilter !(_, lastLm) ![] = [lastLm]
  	doFilter !(lastDesign, lastLm) !(lm:restLms)
	  | lmDesign == lastDesign =
	  	doFilter (lastDesign, chooseLm lastLm lm) restLms
	  | otherwise		   = lastLm:(doFilter (lmDesign, lm) restLms)
	  where lmDesign = MPCObs.getDesign lm
	chooseLm !lm1@(MPCObs.Rec {time=t1}) !lm2@(MPCObs.Rec {time=t2})
	  | t1 <= t2  = lm1
	  | otherwise = lm2
filterObs ![] = []

mZipRecs orbRecs obsRecs = doZipRecs 1 orbRecs obsRecs

doZipRecs !rNum !orbRecs@(orbRec:restOrbRecs) !(obsRec:restObsRecs)
  | orbRecDesign == obsRecDesign = do
	hPutStr stderr $ (replicate 50 '\b') ++ "AstOrb record " ++
		(show rNum) ++ ": " ++ (show orbRecDesign)
	useRecs orbRec obsRec
	doZipRecs (rNum + 1) restOrbRecs restObsRecs
  | otherwise = doZipRecs rNum orbRecs restObsRecs
  where orbRecDesign = MPCObs.getDesign orbRec
	obsRecDesign = MPCObs.getDesign obsRec
doZipRecs !rNum ![] ![] = do
	hPutStrLn stderr $ "\n.Done."
	return (rNum, [])
doZipRecs !rNum ![] _ = do
	hPutStrLn stderr $ "\nRan out of AstOrb records."
	return (rNum, [])
doZipRecs !rNum !orbRecs [] = do
	hPutStrLn stderr $ "\nRan out of MPC observation records."
	return (rNum, orbRecs)


filterZipRecs rNum orbRecs (obsRec:restObsRecs) =
	doFilterZipRecs rNum orbRecs obsRec restObsRecs

doFilterZipRecs rNum oRecs@(orbRec:restOrbRecs) lastObsRec (obsRec:restObsRecs)
  | lastDesign == lmDesign = doFilterZipRecs rNum oRecs chosenObs restObsRecs
  | otherwise = do
  	hPutStr stderr $ (replicate 50 '\b') ++ "Record: " ++ (show rNum)
  	useRecs orbRec lastObsRec
	doFilterZipRecs (rNum + 1) restOrbRecs obsRec restObsRecs
  where lastDesign = MPCObs.getDesign lastObsRec
  	lmDesign = MPCObs.getDesign obsRec
	chosenObs | MPCObs.time lastObsRec <= MPCObs.time obsRec = lastObsRec
		  | otherwise					 = obsRec
doFilterZipRecs rNum oRecs _ [] = do
	hPutStrLn stderr $ "."
	return (rNum, oRecs)

main = do
	progName <- getProgName
	hPutStrLn stderr $ "Hi!  " ++ progName ++ " here!"
	args <- getArgs
	let (astOrbFPath, obsFPaths) = case args of
		[path]   -> (path, ["../t1-read/NumObs.txt.gz",
				    "../t1-read/UnnObs.txt.gz"])
		(path1:rPaths) -> (path1, rPaths)
		_   -> ("../t1-read/astorb.dat.gz", ["../t1-read/NumObs.txt.gz",
			"../t1-read/UnnObs.txt.gz"])
	astOrbBs <- readDecomprFile astOrbFPath
	let astOrbRecs = AstOrb.getRecs astOrbBs
	procsObsFs 1 astOrbRecs obsFPaths
  where procsObsFs _ [] _ = hPutStrLn stderr $ "\nDone."
  	procsObsFs rNum astOrbRecs [] =
  	  hPutStrLn stderr $ "\nRan out of obsFPaths"
  	procsObsFs rNum astOrbRecs (obsFPath:restFPaths) = do
	  hPutStrLn stderr $ "\nProcessing file: " ++ obsFPath
	  obsBs <- readDecomprFile obsFPath
	  let obsRecs = MPCObs.getRecs obsBs
	  (rNum', astOrbRecs') <- filterZipRecs rNum astOrbRecs obsRecs
	  procsObsFs rNum' astOrbRecs' restFPaths
