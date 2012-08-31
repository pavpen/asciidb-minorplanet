-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.Note2
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Names for the codes used in the Note 2 field of minor planet observation
-- records as found in the Minor Planet Center's Observation Archive database.
-- (See <http://www.minorplanetcenter.net/iau/ECS/MPCAT-OBS/MPCAT-OBS.html>.)
--
-- The definitions are from:
--
-- 	<http://www.minorplanetcenter.net/iau/info/OpticalObs.html>
--
-- The creation and maintenance of this module has nothing to do with the Minor
-- Planet Center, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.Note2 where

import Data.Binary.Get			(Get (..), getWord8)
import Data.Word			(Word8)


data ObsType = OpticalObs
	     | PhotographicObs
	     | EncoderObs
	     | CCDObs
	     | MeridianOrTransitCircObs
	     | MicrometerObs
	     | RovingObserverObs
	     | RadarObs
	     | SatelliteObs
	     | CorrectedWithoutRepubCCDObs
	     | OccultationDerivObs
	     | OffsetObs
	     | HipparcosObs
	     | NormalPlaceObs
	     | VideoFrmAvgObs
	     | ReplacedObs Bool
	     deriving (Show, Eq)

mkFromInt  65 = OpticalObs		    -- 'A'
mkFromInt  32 = PhotographicObs		    -- ' '
mkFromInt  80 = PhotographicObs		    -- 'P'
mkFromInt 101 = EncoderObs		    -- 'e'
mkFromInt  67 = CCDObs			    -- 'C'
mkFromInt  84 = MeridianOrTransitCircObs    -- 'T'
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


instance Enum ObsType where
  toEnum = mkFromInt
  fromEnum OpticalObs = 65		    -- 'A'
  fromEnum PhotographicObs = 80		    -- 'P'
  fromEnum EncoderObs = 101		    -- 'e'
  fromEnum CCDObs = 67			    -- 'C'
  fromEnum MeridianOrTransitCircObs = 84    -- 'T'
  fromEnum RovingObserverObs = 86	    -- 'V'
  fromEnum RadarObs = 82		    -- 'R'
  fromEnum SatelliteObs = 83		    -- 'S'
  fromEnum CorrectedWithoutRepubCCDObs = 99 -- 'c'
  fromEnum OccultationDerivObs = 69	    -- 'E'
  fromEnum OffsetObs = 79		    -- 'O'
  fromEnum HipparcosObs = 72		    -- 'H'
  fromEnum NormalPlaceObs = 78		    -- 'N'
  fromEnum VideoFrmAvgObs = 110		    -- 'n'
  fromEnum (ReplacedObs False) = 88	    -- 'X'
  fromEnum (ReplacedObs True) = 120	    -- 'x'


readNote2W8 :: Word8 -> (Bool, ObsType)
readNote2W8 w8 = (w8 == 65 || w8 == 88, mkFromInt w8)


getNote2 :: Get (Bool, ObsType)
getNote2 = do
	fld <- getWord8
	return $ readNote2W8 fld
