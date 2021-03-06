{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    NamedFieldPuns, OverloadedStrings, QuasiQuotes, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import qualified Control.DeepSeq.Generics as DS
import Control.Monad			(forM_, foldM)
import Control.Monad.IO.Class		(liftIO)
import Control.Parallel.Strategies	(NFData, rnf, parMap)
import qualified Data.ASCII.LowellObservatory.AstOrb as AstOrb
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Binary			(encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.List			(isSuffixOf)
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime,
					 diffUTCTime)
import Data.Time.Clock.POSIX		(utcTimeToPOSIXSeconds)
import Data.Time.Calendar		(fromGregorian)
import qualified GHC.Conc as Conc
import qualified MapReduce as MR
import System.Environment		(getProgName, getArgs)
import System.IO			(stderr, hPutStrLn, hPutStr, hClose)
import Text.Printf			(printf)

import Util


bufSize = 1000000 * MPCObs.ChunkSize

bufChunkCnt = Conc.numCapabilities
bufChunkLen = bufSize `div` bufChunkCnt


-- From
-- <http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html>:

data ChunkSpec = ChunkSpec
	       { chunkOfs :: !Int64
	       , chunkLen :: !Int64
	       } deriving (Eq, Show)

withChunks :: (NFData a) => (FilePath -> IO [ChunkSpec])
			 -> ([LCh8.ByteString] -> a)
			 -> FilePath
			 -> IO a
withChunks chunkingFunc processChunkLFunc fPath = do
  (chunks, handles) <- chunkedRead chunkFunc fPath
  let res = processChunkLFunc chunks
  (rnf res `seq` return res) `finally` mapM_ hClose handles


chunkedReadWith :: (NFData a) => ([LCh8.ByteString] -> a) -> FilePath -> IO a
chunkedReadWith processChunkLFunc fPath =
  withChunks (recChunks bufChunkCnt) processChunkLFunc fPath


chunkedRead :: (FilePath -> IO [ChunkSpec]) -> FilePath ->
		IO ([LCh8.ByteString], [Handle])
chunkedRead chunkingFunc fPath = do
  chunks <- chunkingFunc fPath
  liftM unzip . forM chunks $ \spec -> do
    h <- openFile fPath ReadMode
    hSeek h AbsoluteSeek (fromIntegral (chunkOffset spec))
    chunk <- LCh8.take (chunkLength spec) `liftM` LCh8.hGetContents h
    return (chunk, h)


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
	      doProcsRecs (rNum + 1) (chooseObsTSort rec nextRec) restRecs
	  | otherwise = do
	      hPutStr stderr $ (replicate 40 '\b') ++ "Record: " ++ (show rNum)
	      LCh8.putStr $ encode rec
	      doProcsRecs (rNum + 1) nextRec restRecs

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
	hPutStrLn stderr $ ".\nDone."
	return rNum


bufGetChunks bs = procs bufChunkCnt bs
  where procs cntLeft bs
  	  | cntLeft < 1 = [bs]
	  | otherwise = let chunk = obsBufTrimToValid $ BS.take bufChunkLen bs
	  		chunk:(procs (cntLeft - 1) (BS.drop $ BS.length chunk))

procsChunk recs = (length recs, filterObs recs)

-- | 'chLen' = number of records in the chunk.
--   'ch' = the chunk = a sorted list of the filtered records
combineChunks rNum [] = return rNum
combineChunks rNum [(chLen, ch)] = do
	mapM_ (LCh8.putStr . encode) ch
	hPutStr stderr $ (replicate 50 '\b') ++ "Input record: " ++
		(show $ rNum + chLen)
	return (rNum + chLen)
combineChunks rNum ((ch1Len, ch1):(ch2Len, ch2):restChs) = do
	mapM_ (LCh8.putStr . encode) newCh1
	hPutStr stderr $ (replicate 50 '\b') ++ "Input record: " ++
		(show $ rNum + newCh1Len)
	combineChunks (rNum + newCh1Len) $ (newCh2Len, newCh2):restChs
  where lastCh1 = last ch1
  	headCh2 = head ch2
  	(newCh1Len, newCh1, newCh2Len, newCh2) = newCh1_2
	newCh1_2 | null lastCh1 || null headCh2 = (ch1Len, ch1, ch2Len, ch2)
		 | MPCObs.getDesign ch1 == MPCObs.getDesign ch2 =
		 	if MPCObs.time lastCh1 <= MPCObs.time headCh2
			  then (ch1Len, ch1, ch2Len - 1, tail ch2)
			  else (ch1Len - 1, init ch1, ch2Len, ch2)
		 | otherwise = (ch1Len, ch1, ch2Len, ch2)


procsBuf !rNum !bs = do
  let !chunks = DS.force $ bufGetChunks bs
  let res = myMapReduce DS.rnf procsChunk DS.rnf (combineChunks rNum)
  			 (DS.force chunks)
  res `DS.deepseq` return res

filterBS !rNum !bs | LBS.null bs = hPutStrLn stderr $ ".\nDone"
		   | otherwise = do rNum' <- procsBuf buf
				    filterBS rNum' $ LBS.drop (BS.length buf) bs
  where !buf = obsBufTrimToValid $ LBS.toStrict $ LBS.take bufSize bs

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
	  --
	  -- let (obsRec:restObsRecs) = MPCObs.getRecs obsBs
	  -- (rNum', rec') <- foldM mProcsRec (rNum, obsRec) restObsRecs
	  -- hPutStrLn stderr $ (replicate 40 '\b') ++ "Record: " ++ (show rNum')
	  -- 	++ ". [Done.]"
	  -- LCh8.putStr $ encode rec'
	  -- procsObsFs (rNum' + 1) restFPaths
	  rNum' <- filterBS rNum obsBs
	  procsObsFs rNum' restFPaths
