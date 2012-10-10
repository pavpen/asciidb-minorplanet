{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.LowellObservatory.AstOrb
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write asteroid orbit files ('astorb.dat'), The Asteroid Orbital
-- Elements Database, produced by the Lowell Observatory <http://lowell.edu/>.
-- See <ftp://ftp.lowell.edu/pub/elgb/astorb.html>.
--
-- *Acknowledgment and Attribution*
-- The research and computing needed to generate <astorb.dat> were funded
-- principally by NASA grant NAG5-4741, and in part by the Lowell Observatory
-- endowment. <astorb.dat> may be freely used, copied, and transmitted provided
-- attribution to Dr. Edward Bowell and the aforementioned funding sources is
-- made.
--
-- The creation and maintenance of this I/O module has nothing to do with the
-- Lowell Observatory, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.LowellObservatory.AstOrb 
  ( Rec (..)
  , getProvDesign
  , setProvDesign
  , recSize
  , getRec
  , putRec
  , getRecs
  , module Data.ASCII.LowellObservatory.OrbitCrossings
  , module Data.ASCII.LowellObservatory.OrbitUncertainties
  , module Data.ASCII.LowellObservatory.ObjSurveys
  , module Data.ASCII.LowellObservatory.ObjObservations
  , module Data.ASCII.LowellObservatory.ObjDiscoverers
  ) where


import Data.Binary		(Binary (..), decode)
import Data.Binary.Get		(getBytes, getWord8, uncheckedSkip)
import Data.Binary.Put		(putByteString, putWord8)
import qualified Data.ByteString.Char8 as Ch8
import qualified Data.ByteString.Lazy as LBS
import Data.Time		(Day (..))
import Text.Printf		(printf)

import Data.ASCII.Get		(readWithWidth, readWithWidthDeflt,
				 getFloatWithWidth, mayGetFloatWithWidth,
				 getTrimmedStrWithWidth, getJulianYYYYMMDD)
import Data.ASCII.LowellObservatory.OrbitCrossings
				(OrbitCrossings (..), getOrbitCrossings)
import Data.ASCII.LowellObservatory.OrbitUncertainties
				(OrbitUncertainties (..), getOrbitUncertainties)
import Data.ASCII.LowellObservatory.ObjSurveys
				(ObjSurveys (..), getObjSurveys)
import Data.ASCII.LowellObservatory.ObjObservations
				(ObjObservations (..), getObjObservations)
import Data.ASCII.LowellObservatory.ObjDiscoverers
				(ObjDiscoverers (..), getObjDiscoverers)
import Data.ASCII.MinorPlanetCenter.Obj
				(Design (..), Designatable (..))
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.ASCII.Put		(putFloatWithWidth, putJulianYYYYMMDD)



data Rec = Rec
	{ objNumber :: Int				--  1. "A6"
	, designation :: String				--  2. "A18"
	, orbitComputer :: String			--  3. "A15"
	, magnitude :: Double				--  4. "A5"
	, slopeParam :: Double				--  5. "F5.2"
	, colorIndex :: Maybe Double			--  6. "A4"
	, irasDiam :: Maybe Double			--  7. "A5"
	, taxon :: String				--  8. "A4"
	, orbitCrossings :: OrbitCrossings		--  9. "I4"
	, orbitUncertainties :: OrbitUncertainties	--     "I4"
	, surveys :: ObjSurveys				--     "I4"
	, observations :: ObjObservations		--     "I4"
	, discoverers :: ObjDiscoverers			--     "I4"
	, rank :: Int					--     "I4"
	, obsDays :: Int				-- 10. "I5", no "1X"
	, usedObsCnt :: Int				-- 11. "I5"
	, osculationEpoch :: Day			-- 12. "I4,2I2.2"
	, meanAnomRad :: Double				-- 13. "F10.6"
	, argPerihRad :: Double				-- 14. "F10.6"
	, lngAscNodeRad :: Double			-- 15. "F10.6"
	, inclRad :: Double				-- 16. "F9.6"
	, ecc :: Double					-- 17. "F10.8"
	, semiMajAAU :: Double				-- 18. "F12.8"
	, orbitCompDate :: Day				-- 19. "I4,2I2.2"
	, ceuRad :: Double				-- 20. "F7.2"
	, ceuChangeRadPDay :: Double			-- 21. "F8.2"
	, ceuDate :: Day				-- 22. "I4,2I2"
	, peuRad :: Double				-- 23. "F7.2", "X1"
	, peuDate :: Day				--     "I4,2I2"
	, max10yPEURad :: Double			-- 24. "F7.2", "X1"
	, max10yPEUDate :: Day				--     "I4,2I2"
	, peu10yRad :: Double				-- 25. "F7.2", "X1"
	, peu10yDate :: Day				--     "I4,2I2"
	} deriving (Show, Eq)

instance Designatable Rec where
  getDesign (Rec { objNumber, designation })
    | objNumber == -1 = DesignProv $ PD.readLong designation
    | otherwise       = DesignNum objNumber

recSize = 268

putRec (Rec {..}) = do
	putByteString $ Ch8.pack $ if objNumber > 0 then printf "%6d " objNumber
						    else "       "
	putByteString $ Ch8.pack $
	  printf "%-18s %-15s " designation orbitComputer
	putFloatWithWidth 5 magnitude
	putWord8 32 -- ' '
	putFloatWithWidth 5 slopeParam
	putWord8 32 -- ' '
	maybe	(putByteString $ Ch8.pack "    ")
		(\i -> putFloatWithWidth 4 i)
		colorIndex
	putWord8 32 -- ' '
	maybe	(putByteString $ Ch8.pack "     ")
		(\d -> putFloatWithWidth 5 d)
		irasDiam
	putByteString $ Ch8.pack $
	  printf " %-4s %4d%4d%4d%4d%4d%4d %5d%5d "
	  	taxon (fromEnum orbitCrossings)
	  	(fromEnum orbitUncertainties)
		(fromEnum surveys)
		(fromEnum observations)
		(fromEnum discoverers)
		rank obsDays usedObsCnt
	putJulianYYYYMMDD osculationEpoch
	putWord8 32 -- ' '
	putFloatWithWidth 10 (meanAnomRad * 180 / pi)
	putWord8 32 -- ' '
	putFloatWithWidth 10 (argPerihRad * 180 / pi)
	putWord8 32 -- ' '
	putFloatWithWidth 10 (lngAscNodeRad * 180 / pi)
	putWord8 32 -- ' '
	putFloatWithWidth 9 (inclRad * 180 / pi)
	putWord8 32 -- ' '
	putFloatWithWidth 10 ecc
	putWord8 32 -- ' '
	putFloatWithWidth 12 semiMajAAU
	putWord8 32 -- ' '
	putJulianYYYYMMDD orbitCompDate
	putWord8 32 -- ' '
	putFloatWithWidth 7 (ceuRad * 648000 / pi)
	putWord8 32 -- ' '
	putFloatWithWidth 8 (ceuChangeRadPDay * 648000 / pi)
	putWord8 32 -- ' '
	putJulianYYYYMMDD ceuDate
	putWord8 32 -- ' '
	putFloatWithWidth 7 (peuRad * 648000 / pi)
	putWord8 32 -- ' '
	putJulianYYYYMMDD peuDate
	putWord8 32 -- ' '
	putFloatWithWidth 7 (max10yPEURad * 648000 / pi)
	putWord8 32 -- ' '
	putJulianYYYYMMDD max10yPEUDate
	putWord8 32 -- ' '
	putFloatWithWidth 7 (peu10yRad * 648000 / pi)
	putWord8 32 -- ' '
	putJulianYYYYMMDD peu10yDate
	putWord8 10 -- '\n'

getRec = do
	objNumber <- readWithWidthDeflt 6 (-1)
	_ <- getWord8
	designation <- getTrimmedStrWithWidth 18
	_ <- getWord8	
	orbitComputer <- getTrimmedStrWithWidth 15
	_ <- getWord8
	magnitude <- getFloatWithWidth 5
	_ <- getWord8
	slopeParam <- getFloatWithWidth 5
	_ <- getWord8
	colorIndex <- mayGetFloatWithWidth 4
	_ <- getWord8
	irasDiam <- mayGetFloatWithWidth 5
	_ <- getWord8
	taxon <- getTrimmedStrWithWidth 4
	_ <- getWord8
	orbitCrossings <- getOrbitCrossings
	orbitUncertainties <- getOrbitUncertainties
	surveys <- getObjSurveys
	observations <- getObjObservations
	discoverers <- getObjDiscoverers
	rank <- readWithWidth 4
	_ <- getWord8
	obsDays <- readWithWidth 5
	usedObsCnt <- readWithWidth 5
	_ <- getWord8
	osculationEpoch <- getJulianYYYYMMDD
	_ <- getWord8
	meanAnomDeg <- getFloatWithWidth 10
	let meanAnomRad = meanAnomDeg * pi / 180
	_ <- getWord8
	argPerihDeg <- getFloatWithWidth 10
	let argPerihRad = argPerihDeg * pi / 180
	_ <- getWord8
	lngAscNodeDeg <- getFloatWithWidth 10
	let lngAscNodeRad = lngAscNodeDeg * pi / 180
	_ <- getWord8
	inclDeg <- getFloatWithWidth 9
	let inclRad = inclDeg * pi / 180
	_ <- getWord8
	ecc <- getFloatWithWidth 10
	_ <- getWord8
	semiMajAAU <- getFloatWithWidth 12
	_ <- getWord8
	orbitCompDate <- getJulianYYYYMMDD
	_ <- getWord8
	ceuArcSec <- getFloatWithWidth 7
	let ceuRad = ceuArcSec * pi / 648000
	_ <- getWord8
	ceuChangeArcSecPDay <- getFloatWithWidth 8
	let ceuChangeRadPDay = ceuChangeArcSecPDay * pi / 648000
	_ <- getWord8
	ceuDate <- getJulianYYYYMMDD
	_ <- getWord8
	peuArcSec <- getFloatWithWidth 7
	let peuRad = peuArcSec * pi / 648000
	_ <- getWord8
	peuDate <- getJulianYYYYMMDD
	_ <- getWord8
	max10yPEUArcSec <- getFloatWithWidth 7
	let max10yPEURad = max10yPEUArcSec * pi / 648000
	_ <- getWord8
	max10yPEUDate <- getJulianYYYYMMDD
	_ <- getWord8
	peu10yArcSec <- getFloatWithWidth 7
	let peu10yRad = peu10yArcSec * pi / 648000
	_ <- getWord8
	peu10yDate <- getJulianYYYYMMDD
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return $ Rec {..}

instance Binary Rec where
  get = getRec
  put = putRec

getRecs :: LBS.ByteString -> [Rec]
getRecs bs =
	case LBS.null bs of
	  True -> []
	  _    -> let (recBS, bsTail) = LBS.splitAt recSize bs
	  	  in (decode recBS):(getRecs bsTail)

getProvDesign = PD.mayReadLong . designation

setProvDesign rec pDesign = rec { designation = PD.showLong pDesign }
