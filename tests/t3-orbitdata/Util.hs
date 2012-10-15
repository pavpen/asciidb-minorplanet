module Util where

import Codec.Compression.GZip		(decompress)
import Control.Monad			(foldM)
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List			(isSuffixOf)

readDecomprFile path = do
	bs <- LBS.readFile path
	return $ decompr bs
  where decompr bs | ".gz" `isSuffixOf` path = decompress bs
  		   | otherwise		     = bs

chooseObsByTSort rec1 rec2
  | MPCObs.time rec1 <= MPCObs.time rec2  = rec1
  | otherwise = rec2

chooseAObsByTSort aRec1@(rec1, _) aRec2@(rec2, _)
  | MPCObs.time rec1 <= MPCObs.time rec2  = aRec1
  | otherwise = aRec2

filterObs [] = []
filterObs (rec:restRecs) = doFilter (MPCObs.getDesign rec, rec) restRecs
  where doFilter :: (MPCObs.Design, MPCObs.Rec) -> [MPCObs.Rec] -> [MPCObs.Rec]
  	doFilter (_, lastLm) [] = [lastLm]
  	doFilter (lastDesign, lastLm) (lm:restLms)
	  | lmDesign == lastDesign =
	  	doFilter (lastDesign, chooseObsByTSort lastLm lm) restLms
	  | otherwise		   = lastLm:(doFilter (lmDesign, lm) restLms)
	  where lmDesign = MPCObs.getDesign lm

filterAnnotObs [] = []
filterAnnotObs (aRec:restARecs) =
	doFilter (MPCObs.getDesign $ fst aRec, aRec) restARecs
  where doFilter :: (MPCObs.Design, (MPCObs.Rec, MPCObs.Annot))
  		 -> [(MPCObs.Rec, MPCObs.Annot)] -> [(MPCObs.Rec, MPCObs.Annot)]
  	doFilter (_, lastLm) [] = [lastLm]
  	doFilter (lastDesign, lastLm) (lm:restLms)
	  | lmDesign == lastDesign = doFilter (lastDesign, chosenLm) restLms
	  | otherwise		   = lastLm:(doFilter (lmDesign, lm) restLms)
	  where lmDesign = MPCObs.getDesign $ fst lm
	  	chosenLm = chooseAObsByTSort lastLm lm

obsBufTrimToValid buf
  | bufLen < chunkSize = BS.empty
  | testW8 < 97 = buf	-- Note 2 < 'a' (=> is uppercase) => 1-chunk final rec.
  | otherwise = BS.take (bufLen - chunkSize) buf -- Drop 2-chunk final record.
  where bufLen = BS.length buf
	chunkSize = fromIntegral MPCObs.chunkSize
	testW8 = BS.index buf
			  (bufLen - chunkSize + (fromIntegral MPCObs.note2Ofs))
