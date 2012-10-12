{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    NamedFieldPuns, OverloadedStrings, QuasiQuotes, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Monad			(forM_, foldM)
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


readDecomprFile path = do
	bs <- LBS.readFile path
	return $ decompr bs
  where decompr bs | ".gz" `isSuffixOf` path = decompress bs
  		   | otherwise		     = bs

filterObs (rec:restRecs) = doFilter (MPCObs.getDesign rec, rec) restRecs
  where doFilter :: (MPCObs.Design, MPCObs.Rec) -> [MPCObs.Rec] -> [MPCObs.Rec]
  	doFilter (_, lastLm) [] = [lastLm]
  	doFilter (lastDesign, lastLm) (lm:restLms)
	  | lmDesign == lastDesign =
	  	doFilter (lastDesign, chooseLm lastLm lm) restLms
	  | otherwise		   = lastLm:(doFilter (lmDesign, lm) restLms)
	  where lmDesign = MPCObs.getDesign lm
	chooseLm lm1@(MPCObs.Rec {time=t1}) lm2@(MPCObs.Rec {time=t2})
	  | t1 <= t2  = lm1
	  | otherwise = lm2
filterObs [] = []

procsRecs rNum [] = do
	hPutStrLn stderr $ "\n.Done"
	return rNum
procsRecs rNum (obsRec:restObsRecs) = doProcsRecs rNum obsRec restObsRecs
  where doProcsRecs rNum rec [] = do
  	  LCh8.putStr $ encode rec
	  hPutStrLn stderr $ "\n.Done"
	  return rNum
	doProcsRecs rNum rec (nextRec:restRecs)
	  | MPCObs.getDesign rec == MPCObs.getDesign nextRec =
	      doProcsRecs (rNum + 1) (chooseRec rec nextRec) restRecs
	  | otherwise = do
	      hPutStr stderr $ (replicate 40 '\b') ++ "Record: " ++ (show rNum)
	      LCh8.putStr $ encode rec
	      doProcsRecs (rNum + 1) nextRec restRecs
	chooseRec rec1@(MPCObs.Rec {time=t1}) rec2@(MPCObs.Rec {time=t2})
	  | t1 <= t2  = rec1
	  | otherwise = rec2

mProcsRec (rNum, rec) nextRec
  | MPCObs.getDesign rec == MPCObs.getDesign nextRec =
      return (rNum + 1, chosenRec)
  | otherwise = do
      hPutStr stderr $ (replicate 40 '\b') ++ "Record: " ++ (show rNum)
      LCh8.putStr $ encode rec
      return (rNum + 1, nextRec)
  where chosenRec | MPCObs.time rec <= MPCObs.time nextRec  = rec
		  | otherwise = nextRec

doProcsRecs' rNum (obsRec:restObsRecs) = do
	LCh8.putStr $ encode obsRec
	doProcsRecs' (rNum + 1) restObsRecs
doProcsRecs' rNum _ = do
	hPutStrLn stderr $ "\n.Done."
	return rNum

main = do
	progName <- getProgName
	hPutStrLn stderr $ "Hi!  " ++ progName ++ " here!"
	args <- getArgs
	let obsFPaths = case args of
		[]     -> ["../t1-read/NumObs.txt.gz",
				    "../t1-read/UnnObs.txt.gz"]
		rPaths -> rPaths
	procsObsFs 1 obsFPaths
  where procsObsFs rNum [] = hPutStrLn stderr $ "\nDone."
  	procsObsFs rNum (obsFPath:restFPaths) = do
	  hPutStrLn stderr $ "\nProcessing file: " ++ obsFPath
	  obsBs <- readDecomprFile obsFPath
	  -- let obsRecs = MPCObs.getRecs obsBs
	  -- rNum' <- procsRecs rNum obsRecs
	  let (obsRec:restObsRecs) = MPCObs.getRecs obsBs
	  (rNum', rec') <- foldM mProcsRec (rNum, obsRec) restObsRecs
	  hPutStrLn stderr $ (replicate 40 '\b') ++ "Record: " ++ (show rNum')
	  	++ ". [Done.]"
	  LCh8.putStr $ encode rec'
	  procsObsFs (rNum' + 1) restFPaths
