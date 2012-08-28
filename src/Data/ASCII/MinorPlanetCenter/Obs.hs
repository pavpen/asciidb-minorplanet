{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.Obs
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write minor planet observation records as found in the Minor Planet
-- Center's Observation Archive database.  (See
-- <http://www.minorplanetcenter.net/iau/ECS/MPCAT-OBS/MPCAT-OBS.html>.)
--
-- Some descriptions of the format are:
--
-- 	* <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>
--
-- 	* <http://www.minorplanetcenter.net/iau/info/PackedDes.html>
--
-- 	* <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>
--
-- The creation and maintenance of this module has nothing to do with the Minor
-- Planet Center, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.Obs
  ( Rec (..)
  , recSize
  , putRec
  , getRec
  , mayGetRec
  , getRecs
  , getMayRecs
  module Data.ASCII.MinorPlanetCenter.ProvisionalDesignations
  ) where


import Data.Binary		(Binary (..), decode, get)
import Data.Binary.Get		(getBytes, getByteString, getLazyByteString,
				 getWord8, isEmpty, runGet)
import Data.Binary.Put		(putByteString, putWord8)
import qualified Data.ByteString.Char8 as Ch8
import qualified Data.ByteString.Lazy as LBS
import Data.Time		(UTCTime)
import Safe			(readMay)
import Text.Printf		(printf)

import Data.ASCII.Get		(readWithWidth, readWithWidthDeflt,
				 mayReadWithWidth, getEnumWord8,
				 getStrWithWidth, getTrimmedStrWithWidth)
import Data.ASCII.MinorPlanetCenter.ProvisionalDesignations
				(ProvDesign (..))
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.ASCII.MinorPlanetCenter.Util
				(getTime, putTime, getRightAscSec,
				 putRightAscSec, getDeclRad, putDeclRad,
				 mayGetRightAscSec, mayGetDeclRad)


data Rec = Rec
	{ objNumber :: Int
	, provDesign :: Maybe ProvDesign
	, discovery :: Bool
	, note1 :: Char
	, note2 :: Char
	, time :: UTCTime
	, rightAscSec :: Double
	, declRad :: Double
	, magnitude :: Maybe Double
	, obsBand :: Char
	, rfcCode :: String
	, observatoryCode :: String
	} deriving (Show)


recSize = 81


putRec (Rec {..}) = do
        if objNumber > 0 then putByteString $ Ch8.pack $ printf "%05d" objNumber
			 else putByteString $ Ch8.pack "     "
	putByteString $ Ch8.pack $
		maybe "       " (\d -> PD.showPacked d) provDesign
	putWord8 $ if discovery then 42 else 32 -- '*' or ' '
	putWord8 $ fromIntegral $ fromEnum note1
	putWord8 $ fromIntegral $ fromEnum note2
	putTime time
	putRightAscSec rightAscSec
	putDeclRad declRad
	putByteString $ Ch8.pack "         "
	putByteString $ Ch8.pack $ maybe "     "
					 (\m -> printf "%05.2f" m)
					 magnitude
	putWord8 $ fromIntegral $ fromEnum obsBand
	putWord8 32 -- ' '
	putByteString $ Ch8.pack $ rfcCode
	putByteString $ Ch8.pack $ if null observatoryCode then "   "
					                   else observatoryCode
	putWord8 10 -- Terminating line-feed character.

getRec = do
  	objNumber <- readWithWidthDeflt 5 (-1)
	provDesign <- PD.mayGetPacked
	discoveryBs <- getWord8
	let discovery = discoveryBs == 42
	note1 <- getEnumWord8
	note2 <- getEnumWord8
	time <- getTime
	rightAscSec <- getRightAscSec
	declRad <- getDeclRad
	_ <- getBytes 9
	magnitude <- mayReadWithWidth 5
	obsBand <- getEnumWord8
	_ <- getWord8
	rfcCode <- getStrWithWidth 5
	observatoryCode <- getTrimmedStrWithWidth 3
	return Rec {..}

mayGetRec = do
  	objNumber <- readWithWidthDeflt 5 (-1)
	provDesign <- PD.mayGetPacked
	discoveryBs <- getWord8
	let discovery = discoveryBs == 42
	note1 <- getEnumWord8
	note2 <- getEnumWord8
	time <- getTime
	rightAscSecM <- mayGetRightAscSec
	maybe (return Nothing)
	 (\rightAscSec -> do
	  declRadM <- mayGetDeclRad
	  maybe (return Nothing)
	   (\declRad -> do
		  _ <- getBytes 9
		  magnitude <- mayReadWithWidth 5
		  obsBand <- getEnumWord8
		  _ <- getWord8
		  rfcCode <- getStrWithWidth 5
		  observatoryCode <- getTrimmedStrWithWidth 3
		  return $ Just $ Rec  {..}
	   )
	   declRadM
	 )
	 rightAscSecM


instance Binary Rec where
  get = getRec
  put = putRec

--getRecs = do
--	eos <- isEmpty
--	case eos of
--	  True -> return []
--	  _    -> do	bs <- getLazyByteString recSize
--	  		tail <- getRecs
--	  		return $ (decode bs):tail

getRecs :: LBS.ByteString -> [Rec]
getRecs bs =
	case LBS.null bs of
	  True -> []
	  _    -> let (rec, recTail) = LBS.splitAt recSize bs
	          in (decode rec):(getRecs recTail)

getMayRecs :: LBS.ByteString -> [Rec]
getMayRecs bs =
	case LBS.null bs of
	  True -> []
	  _    -> let (rec, recTail) = LBS.splitAt recSize bs
	  	      recsTail = getMayRecs recTail
	          in maybe recsTail
		  	   (\rec -> rec:(recsTail))
			   (runGet mayGetRec bs)
