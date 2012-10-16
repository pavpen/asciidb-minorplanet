{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    NamedFieldPuns, OverloadedStrings, QuasiQuotes, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Monad			(forM_, foldM)
import Control.Monad.IO.Class		(liftIO)
import qualified Data.ASCII.LowellObservatory.AstOrb as AstOrb
import qualified Data.ASCII.MinorPlanetCenter.Design as D
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
import FilterObsCmdLnOpts		(CmdLnOpts (..), parseArgV)


chooseARec False aRec1@(rec1, _) aRec2@(rec2, _)
  | MPCObs.time rec1 <= MPCObs.time rec2 = aRec1
  | otherwise				 = aRec2

chooseARec True aRec1@(rec1, _) aRec2@(rec2, _)
  | MPCObs.discovery rec1		 = aRec1
  | MPCObs.discovery rec2		 = aRec2
  | MPCObs.time rec1 <= MPCObs.time rec2 = aRec1
  | otherwise				 = aRec2

mProcsRec filtDiscov (rNum, aRec@(rec, MPCObs.Annot {..})) nextARec@(nextRec, _)
  | MPCObs.getDesign rec == MPCObs.getDesign nextRec =
      return (rNum + 1, chooseARec filtDiscov aRec nextARec)
  | otherwise = do
      hPutStr stderr $ (replicate 60 '\b') ++ "Record: " ++ (show rNum) ++
      		", lines: " ++ (show lineNums)
      if filtDiscov && not (MPCObs.discovery rec)
        then hPutStrLn stderr $ ": " ++ (D.showPacked $ MPCObs.getDesign rec) ++
		": No discovery observation found for this object!"
	else hPutStr stderr $ replicate 10 ' '
      LCh8.putStr $ srcBytes
      return (rNum + 1, nextARec)

main = do
	progName <- getProgName
	hPutStrLn stderr $ "Hi!  " ++ progName ++ " here!"
	args <- getArgs
	(CmdLnOpts {..}, obsFPaths) <- parseArgV args
	hPutStrLn stderr $ "Filter by \"discovery\" flag: " ++
		(show filtDiscovFlag)
	if outputUsage then procsObsFs filtDiscovFlag 1 obsFPaths
		       else return ()
  where procsObsFs filtDiscov rNum [] = hPutStrLn stderr $ "\nDone."
  	procsObsFs filtDiscov rNum (obsFPath:restFPaths) = do
	  hPutStrLn stderr $ "\nProcessing file: " ++ obsFPath
	  obsBs <- readDecomprFile obsFPath
	  let (obsRec:restObsRecs) = MPCObs.getAnnotRecs 1 obsBs
	  (rNum', rec') <- foldM (mProcsRec filtDiscov)
	  			 (rNum, obsRec) restObsRecs
	  hPutStrLn stderr $ (replicate 40 '\b') ++ "Record: " ++ (show rNum')
	  	++ ". [Done.]          "
	  LCh8.putStr $ MPCObs.srcBytes $ snd rec'
	  procsObsFs filtDiscov (rNum' + 1) restFPaths
