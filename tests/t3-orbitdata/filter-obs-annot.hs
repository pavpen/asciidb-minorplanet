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

import Util


mProcsRec (rNum, aRec@(rec, MPCObs.Annot {..})) nextARec@(nextRec, _)
  | MPCObs.getDesign rec == MPCObs.getDesign nextRec =
      return (rNum + 1, chosenARec)
  | otherwise = do
      hPutStr stderr $ (replicate 60 '\b') ++ "Record: " ++ (show rNum) ++
      		", lines: " ++ (show lineNums) ++ (replicate 10 ' ')
      -- LCh8.putStr $ encode rec
      LCh8.putStr $ srcBytes
      return (rNum + 1, nextARec)
  where chosenARec | MPCObs.time rec <= MPCObs.time nextRec  = aRec
		   | otherwise = nextARec

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
	  let (obsRec:restObsRecs) = MPCObs.getAnnotRecs 1 obsBs
	  (rNum', rec') <- foldM mProcsRec (rNum, obsRec) restObsRecs
	  hPutStrLn stderr $ (replicate 40 '\b') ++ "Record: " ++ (show rNum')
	  	++ ". [Done.]          "
	  LCh8.putStr $ MPCObs.srcBytes $ snd rec'
	  procsObsFs (rNum' + 1) restFPaths
