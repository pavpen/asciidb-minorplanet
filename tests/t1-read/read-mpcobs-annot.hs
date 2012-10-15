{-# LANGUAGE RecordWildCards #-}

import Codec.Compression.GZip	(decompress)
import Data.Binary		(decode, encode)
import Data.Binary.Get		(runGet)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.List		(isSuffixOf)
import System.Environment	(getArgs)

import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD



main = do
	args <- getArgs
	let inpFPath = case args of
			  (path:_) -> path
			--  _        -> "NumObs.txt.gz"
			  _        -> "UnnObs.txt.gz"
	let decompressor | ".gz" `isSuffixOf` inpFPath = decompress
			 | otherwise = id
	bs <- LBS.readFile inpFPath
	putStrLn $ show $ runGet (MPCObs.getAnnotRecChunk 1) $ decompressor bs
	putStrLn $ "Reading: " ++ inpFPath
	putStrLn ""
	mapM_ procsAnnotRec (MPCObs.getAnnotRecs 1 $ decompressor bs)
  where procsAnnotRec (rec, MPCObs.Annot {..}) = do
  	  putStrLn $ "Lines: " ++ (show lineNums)
	  LBS.putStr $ srcBytes
	  LCh8.putStrLn $ encode rec
