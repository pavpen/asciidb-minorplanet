{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.Observer
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write the note 2, observation, and observatory fields for minor
-- planet observation records as found in the Minor Planet Center's Observation
-- Archive database.  (See
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
-- 	* <http://www.minorplanetcenter.net/iau/info/RovingObs.html>
--
-- The creation and maintenance of this module has nothing to do with the Minor
-- Planet Center, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.Observer where

import Data.ASCII.Get		(mayReadWithWidth, getEnumWord8,
				 getStrWithWidth)
import Data.ASCII.MinorPlanetCenter.Observatory
				(Observatory (..))
import Data.ASCII.MinorPlanetCenter.Util
				(getRightAscSec, putRightAscSec, getDeclRad,
				 putDeclRad)
import Data.Binary.Get		(Get (..), getWord8, getBytes)
import Data.Binary.Put		(putWord8, putByteString)
import qualified Data.ByteString.Char8 as Ch8
import Data.Conversion		(SIConvertible (..))
import Data.Word		(Word8)
import Text.Printf		(printf)



data ObsData = ObsData
	    { rightAscSec :: Double
	    , declRad :: Double
	    , magnitude :: Maybe Double
	    , obsBand :: Char
	    , rfcCode :: String
	    } deriving (Show, Eq)

data SigReturnPt = RetFromSurface
		 | RetFromCtrMass
		 deriving (Show, Eq)

instance Enum SigReturnPt where
  toEnum 83 = RetFromSurface -- 'S'
  toEnum 67 = RetFromCtrMass -- 'C'
  fromEnum RetFromSurface = 83  -- 'S'
  fromEnum RetFromCtrMass = 67  -- 'C'


data SatObsUnitsToUse = MetersSatU
		      | AUSatU
		      deriving (Show, Eq)

instance Enum SatObsUnitsToUse where
  toEnum 49 = MetersSatU  -- '1'
  toEnum 50 = AUSatU	  -- '2'
  fromEnum MetersSatU = 49	-- '1'
  fromEnum AUSatU = 50		-- '2'

instance SIConvertible SatObsUnitsToUse where
  toSIFactor MetersSatU = 1
  toSIFactor AUSatU = 149597870700


data Observer = OpticalObs	    { obsData :: ObsData }
	      | PhotographicObs	    { obsData :: ObsData }
	      | EncoderObs	    { obsData :: ObsData }
	      | CCDObs		    { obsData :: ObsData }
	      | MeridianOrTransitCircObs
	      			    { obsData :: ObsData }
	      | MicrometerObs	    { obsData :: ObsData }
	      | RovingObserverObs   { obsData :: ObsData }
	      | RadarObs	    { sigSrcObservatory :: Observatory
	      			    , sigTimeDelayMuS :: Maybe Double
				    , sigTimeDelayErrMuS :: Maybe Double
				    , dopplerShiftHz :: Maybe Double
				    , dopplerShiftErrHz :: Maybe Double
				    , sigFreqHz :: Double
				    , sigReturnPt :: SigReturnPt
				    , obsRfcCode :: String
				    }
	      | SatelliteObs	    { obsData :: ObsData
	      			    , parallaxM :: (Double, Double, Double)
	      			    , unitsToUse :: SatObsUnitsToUse
				    }
	      | CorrectedWithoutRepubCCDObs
	      			    { obsData :: ObsData }
	      | OccultationDerivObs { obsData :: ObsData }
	      | OffsetObs	    { obsData :: ObsData }
	      | HipparcosObs	    { obsData :: ObsData }
	      | NormalPlaceObs	    { obsData :: ObsData }
	      | VideoFrmAvgObs	    { obsData :: ObsData }
	      | ReplacedObs	    { obsData :: ObsData
	      			    , lowercaseX :: Bool }
	      deriving (Show, Eq)

{-
mkFromInt  65 = OpticalObs		    -- 'A'
mkFromInt  32 = PhotographicObs		    -- ' '
mkFromInt  80 = PhotographicObs		    -- 'P'
mkFromInt 101 = EncoderObs		    -- 'e'
mkFromInt  67 = CCDObs			    -- 'C'
mkFromInt  84 = MeridianOrTransitCircObs    -- 'T'
mkFromInt  77 = MicrometerObs		    -- 'M'
mkFromInt  86 = RovingObserverObs	    -- 'V'
mkFromInt 118 = RovingObserverObs	    -- 'v'
mkFromInt  82 = RadarObs		    -- 'R'
mkFromInt 114 = RadarObs		    -- 'r'
mkFromInt  83 = SatelliteObs		    -- 'S'
mkFromInt 115 = SatelliteObs		    -- 's'
mkFromInt  99 = CorrectedWithoutRepubCCDObs -- 'c'
mkFromInt  69 = OccultationDerivObs	    -- 'E'
mkFromInt  79 = OffsetObs		    -- 'O'
mkFromInt  72 = HipparcosObs		    -- 'H'
mkFromInt  78 = NormalPlaceObs		    -- 'N'
mkFromInt 110 = VideoFrmAvgObs		    -- 'n'
mkFromInt  88 = ReplacedObs False	    -- 'X'
mkFromInt 120 = ReplacedObs True	    -- 'x'
-}


obsTypeNum (OpticalObs {..})			=  65 -- 'A'
obsTypeNum (PhotographicObs {..})		=  80 -- 'P'
obsTypeNum (EncoderObs {..})			= 101 -- 'e'
obsTypeNum (CCDObs {..})			=  67 -- 'C'
obsTypeNum (MeridianOrTransitCircObs {..})	=  84 -- 'T'
obsTypeNum (MicrometerObs {..})			=  77 -- 'M'
obsTypeNum (RovingObserverObs {..})		=  86 -- 'V'
obsTypeNum (RadarObs {..})			=  82 -- 'R'
obsTypeNum (SatelliteObs {..})			=  83 -- 'S'
obsTypeNum (CorrectedWithoutRepubCCDObs {..})	=  99 -- 'c'
obsTypeNum (OccultationDerivObs {..})		=  69 -- 'E'
obsTypeNum (OffsetObs {..})			=  79 -- 'O'
obsTypeNum (HipparcosObs {..})			=  72 -- 'H'
obsTypeNum (NormalPlaceObs {..})		=  78 -- 'N'
obsTypeNum (VideoFrmAvgObs {..})		= 110 -- 'n'
obsTypeNum (ReplacedObs {lowercaseX=False, ..}) =  88 -- 'X'
obsTypeNum (ReplacedObs {lowercaseX=True, ..}) 	= 120 -- 'x'


note2W8GetJ2000Adj :: Word8 -> Bool
note2W8GetJ2000Adj w8 = w8 == 65 || w8 == 88

getObsData = do
	rightAscSec <- getRightAscSec	-- Col. 33-44
	declRad <- getDeclRad		-- Col. 45-56
	_ <- getBytes 9			-- Col. 57-65
	magnitude <- mayReadWithWidth 5 -- Col. 66-70
	obsBand <- getEnumWord8		-- Col. 71
	_ <- getWord8			-- Col. 72
	rfcCode <- getStrWithWidth 5	-- Col. 73-77
	return $ ObsData {..}

putObsData (ObsData {..}) = do
	putRightAscSec rightAscSec	-- Col. 33-44
	putDeclRad declRad		-- Col. 45-56
	putByteString $ Ch8.pack "         "	-- Col. 57-65
	putByteString $ Ch8.pack $ maybe "     "
					 (\m -> printf "%05.2f" m)
					 magnitude
	putWord8 $ fromIntegral $ fromEnum obsBand	-- Col. 71
	putWord8 32 -- ' '		-- Col. 72
	putByteString $ Ch8.pack $ rfcCode	-- col. 73-77
