{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    NamedFieldPuns, OverloadedStrings, QuasiQuotes, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Monad			(forM_)
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
import System.IO			(stderr, hPutStr, hPutStrLn)


readDecomprFile path = do
	bs <- LBS.readFile path
	return $ decompr bs
  where decompr bs | ".gz" `isSuffixOf` path = decompress bs
  		   | otherwise		     = bs

filterNumObs recs = doFilter (MPCObs.DesignNum (-1)) recs
  where doFilter :: MPCObs.Design -> [MPCObs.Rec] -> [MPCObs.Rec]
  	doFilter _ [] = []
  	doFilter lastDesign (lm:restLms)
	  | lmDesign == lastDesign = doFilter lastDesign restLms
	  | otherwise		   = lm:(doFilter lmDesign restLms)
	  where lmDesign = MPCObs.getDesign lm

verifyNumObs (rec:[]) lnNum = do
	putStrLn $ "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\bLine: " ++ (show lnNum) ++ "."
	hPutStrLn stderr $ "verifyNumObs: Done."
verifyNumObs (pRec:restRecs@(nRec:_)) lnNum
  | MPCObs.objNumber pRec == MPCObs.objNumber nRec = testTime
  | MPCObs.objNumber pRec <  MPCObs.objNumber nRec = passTest
  | otherwise                                      = do
      hPutStrLn stderr $ "verifyNumObs: Bad 'objNumber' order on line " ++
      			  (show lnNum) ++ "!"
      hPutStr   stderr $ LCh8.unpack $ encode pRec
      hPutStrLn stderr $ LCh8.unpack $ encode nRec
      passTest
  where testTime | MPCObs.time pRec <= MPCObs.time nRec = passTest
  		 | otherwise                            = do
		     hPutStrLn stderr $"verifyNumObs: Bad 'time' order on line "
		    			++ (show lnNum) ++ "!"
		     hPutStr   stderr $ LCh8.unpack $ encode pRec
		     hPutStrLn stderr $ LCh8.unpack $ encode nRec
		     passTest
	passTest = do
		putStr $ "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\bLine: " ++ (show lnNum)
		verifyNumObs restRecs (lnNum + (MPCObs.recGetChunkCnt pRec))

verifyUnnObs (rec:[]) lnNum = do
	putStrLn $ "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\bLine: " ++ (show lnNum) ++ "."
	hPutStrLn stderr $ "verifyUnnObs: Done."
verifyUnnObs (pRec:restRecs@(nRec:_)) lnNum
  | MPCObs.provDesign pRec == Nothing = do
      hPutStrLn stderr $ "verifyUnnObs: Nothing 'provDesign' on line "
			  ++ (show lnNum) ++ "!"
      hPutStrLn stderr $ LCh8.unpack $ encode pRec
      passTest
  | MPCObs.provDesign pRec == MPCObs.provDesign nRec = testTime
  | otherwise					     =
      case (MPCObs.provDesign pRec, MPCObs.provDesign nRec) of
        (Just pPD, Just nPD) -> testProvDesign pPD nPD
	_ -> do
	--      hPutStrLn stderr $ "verifyUnnObs: Nothing 'provDesign' on line "
	--      			  ++ (show $ lnNum + MPCObs.recGetChunkCnt pRec)
	--			  ++ "!"
	--      hPutStrLn stderr $ LCh8.unpack $ encode nRec
	      passTest
  where testTime | MPCObs.time pRec <= MPCObs.time nRec = passTest
  		 | otherwise				= do
		     hPutStrLn stderr $"verifyUnnObs: Bad 'time' order on line "
 					++ (show lnNum) ++ "!"
		     hPutStr   stderr $ LCh8.unpack $ encode pRec
		     hPutStrLn stderr $ LCh8.unpack $ encode nRec
		     passTest
	testProvDesign pPD nPD
	  | (PD.showPacked pPD) < (PD.showPacked nPD) = passTest
	  | otherwise				      = do
	      hPutStrLn stderr $ "verifyUnnObs: Bad 'provDesign' order on line "
	      			  ++ (show lnNum) ++ "!"
	      hPutStr   stderr $ LCh8.unpack $ encode pRec
	      hPutStrLn stderr $ LCh8.unpack $ encode nRec
	      passTest
	passTest = do
		putStr $ "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\bLine: " ++ (show lnNum)
		verifyUnnObs restRecs (lnNum + (MPCObs.recGetChunkCnt pRec))

useRecs [] = hPutStrLn stderr $ "Empty input!"
useRecs recs@(MPCObs.Rec {objNumber=(-1)}:_) = verifyUnnObs recs 1
useRecs recs = verifyNumObs recs 1

main = do
	progName <- getProgName
	putStrLn $ "Hi!  " ++ progName ++ " here!"
	args <- getArgs
	let inpFPath = case args of
		(path:_) -> path
		_        -> "../t1-read/NumObs.txt.gz"
	inpBs <- readDecomprFile inpFPath
	useRecs $ MPCObs.getRecs inpBs
