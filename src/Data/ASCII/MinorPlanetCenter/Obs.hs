{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, NamedFieldPuns,
    RecordWildCards #-}

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
  , chunkSize
  , recGetChunkCnt
  , putRec
  , getRec
  , getRecs
  , module Data.ASCII.MinorPlanetCenter.Obj
  , module Data.ASCII.MinorPlanetCenter.Observer
  , module Data.ASCII.MinorPlanetCenter.Observatory
  , module Data.ASCII.MinorPlanetCenter.ProvisionalDesignations
  ) where


import Control.Monad		(mapM_)
import Control.Monad.Loops	(untilM)
import Data.Binary		(Binary (..), decode, get)
import Data.Binary.Get		(Get (..), getBytes, getByteString,
				 getLazyByteString, getWord8, isEmpty, runGet,
				 uncheckedSkip, skip, runGetState)
import Data.Binary.Put		(putByteString, putWord8)
import qualified Data.ByteString.Char8 as Ch8
import qualified Data.ByteString.Lazy as LBS
import Data.Time		(UTCTime)
import Data.Word		(Word8)
import Safe			(readMay)
import Text.Printf		(printf)

import Data.ASCII.Get		(readWithWidth, readWithWidthDeflt,
				 mayReadWithWidth, getEnumWord8,
				 getStrWithWidth, getTrimmedStrWithWidth,
				 getFloatWithWidth, getFloatImplFPWithWidth,
				 mayGetFloatImplFPWithWidth,
				 getDecFracWithWidth', getAlNumWithWidthDeflt)
import Data.ASCII.MinorPlanetCenter.Obj
				(Design (..), Designatable (..))
import Data.ASCII.MinorPlanetCenter.Observer
				(Observer (..), ObsData (..),
				 SatObsUnitsToUse (..), SigReturnPt (..),
				 obsTypeNum, note2W8GetJ2000Adj, getObsData,
				 putObsData)
import Data.ASCII.MinorPlanetCenter.Observatory
				(Observatory (..), observatoryGetCode)
import Data.ASCII.MinorPlanetCenter.ProvisionalDesignations
				(ProvDesign (..))
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.ASCII.MinorPlanetCenter.Util
				(getTime, putTime, getRightAscSec,
				 putRightAscSec, getDeclRad, putDeclRad,
				 mayGetRightAscSec, mayGetDeclRad)
import Data.ASCII.Put		(putFloatWithWidth, putAlNumWithWidth)
import Data.Conversion		(SIConvertible (..))
import Data.Typeable		(Typeable)


data Rec = Rec
	{ objNumber :: Int
	, provDesign :: Maybe ProvDesign
	, discovery :: Bool
	, note1 :: Char
	, j2000Adj :: Bool
	, time :: UTCTime
	, observer :: Observer
	, observatory :: Observatory
	} deriving (Eq, Show, Typeable)

instance Designatable Rec where
  getDesign (Rec { objNumber=(-1), provDesign=(Just pd)}) = DesignProv pd
  getDesign rec@(Rec { objNumber })
    | objNumber /= -1 = DesignNum objNumber
    | otherwise       = error $ "getDesign: Undesignatable object: "++(show rec)


chunkSize = 81

recGetChunkCnt (Rec {observer = RovingObserverObs {..}}) = 2
recGetChunkCnt (Rec {observer = RadarObs {..}}) = 2
recGetChunkCnt (Rec {observer = SatelliteObs {..}}) = 2
recGetChunkCnt _ = 1


putRecObserv (Rec { objNumber
		  , provDesign
		  , discovery
		  , note1
		  , j2000Adj
		  , time
		  , observer = observer@(RovingObserverObs {..})
		  , observatory = observatory@(RovingObserver {..}) }) = do
	let (ObsData {..}) = obsData
	putObsData obsData
	putByteString $ Ch8.pack $ observatoryGetCode observatory -- Col. 78-80
	putWord8 10 -- Terminating line-feed character.
        if objNumber > 0 then putAlNumWithWidth 5 objNumber
			 else putByteString $ Ch8.pack "     "
	putByteString $ Ch8.pack $		-- Rec. 2, Col.  6-12
		maybe "       " (\d -> PD.showPacked d) provDesign
	putWord8 $ if discovery then 42 else 32 -- '*' or ' '	Col. 13
	putWord8 $ fromIntegral $ fromEnum note1 --Rec. 2, Col. 14
	putWord8 118 -- 'v'			-- Rec. 2, Col. 15
	putTime time				-- Rec. 2, Col. 16-32
	putWord8  49 -- '1'			-- Rec. 2, Col. 33
	putWord8  32 -- ' '			-- Rec. 2, Col. 34
	--putFloatWithWidth 10 (lonRad * 180 / pi)-- Rec. 2, Col. 35-44
	putByteString $ Ch8.pack $ printf "%010.6f" (lonRad * 180 / pi)
	putWord8  32 -- ' '			-- Rec. 2, Col. 45
	--putFloatWithWidth 10 (latRad * 180 / pi)-- Rec. 2, Col. 46-55
	putByteString $ Ch8.pack $ printf "%+010.6f" (latRad * 180 / pi)
	putWord8  32 -- ' '			-- Rec. 2, Col. 56
	putByteString $ Ch8.pack $ printf "%5d" (round altMeters :: Int)
	putByteString $ Ch8.pack "           "	-- Rec. 2, Col. 62-73
	putByteString $ Ch8.pack rfcCode	-- Rec. 2, Col. 73-77
	putByteString $ Ch8.pack "247"		-- Rec. 2, Col. 78-80
	putWord8 10 -- Terminating line-feed character.
putRecObserv (Rec { objNumber
		  , provDesign
		  , discovery
		  , note1
		  , j2000Adj
		  , time
		  , observer = RadarObs {..}
		  , observatory }) = do
	maybe (putByteString $ Ch8.pack "               ")
	      (\v -> do let (sigTD1,'.':sigTD2) = splitAt 11 $printf "%016.4f" v
		 	putByteString $ Ch8.pack sigTD1	-- Col. 33-43
		 	putByteString $ Ch8.pack sigTD2	-- Col. 44-47
	      )
	      sigTimeDelayMuS
	maybe (putByteString $ Ch8.pack "               ")
	      (\v -> do let (dopSh1,'.':dopSh2)=splitAt 11 $ printf "%+016.4f" v
			putByteString $ Ch8.pack dopSh1	-- Col. 48-58
			putByteString $ Ch8.pack dopSh2	-- Col. 59-62
	      )
	      dopplerShiftHz
	let (sigFreq1, '.':sigFreq2:sigFreq3) = splitAt 5 $
			printf "%013.7f" $ sigFreqHz / 1000000
	putByteString $ Ch8.pack sigFreq1		-- Col. 63-67
	putWord8 $ fromIntegral $ fromEnum sigFreq2	-- Col. 68
	putByteString $ Ch8.pack $ observatoryGetCode sigSrcObservatory -- 69-71
	putWord8  32 -- ' '				-- Col. 72
	putByteString $ Ch8.pack obsRfcCode		-- Col. 73-77
	putByteString $ Ch8.pack $ observatoryGetCode observatory -- Col. 78-80
	putWord8 10 -- Terminating line-feed character.
        if objNumber > 0 then putAlNumWithWidth 5 objNumber
			 else putByteString $ Ch8.pack "     "
	putByteString $ Ch8.pack $		-- Rec. 2, Col.  6-12
		maybe "       " (\d -> PD.showPacked d) provDesign
	putWord8 $ if discovery then 42 else 32 -- '*' or ' '	Col. 13
	putWord8 $ fromIntegral $ fromEnum note1 --Rec. 2, Col. 14
	putWord8 114 -- 'r'			-- Rec. 2, Col. 15
	putTime time				-- Rec. 2, Col. 16-32
	putWord8 $ fromIntegral $ fromEnum sigReturnPt	-- Rec. 2, Col. 33
	maybe (putByteString $ Ch8.pack "              ")
	      (\v -> do let (sigTDErr1, '.':sigTDErr2) = splitAt 10 $
				printf "%015.4f" v
			putByteString $ Ch8.pack sigTDErr1 -- Rec. 2, Col. 34-43
			putByteString $ Ch8.pack sigTDErr2 -- Rec. 2, Col. 44-47
	      )
	      sigTimeDelayErrMuS
	maybe (putByteString $ Ch8.pack "               ")
	      (\v -> do let (dopShErr1, '.':dopShErr2) = splitAt 11 $
				printf "%016.4f" v
			putByteString $ Ch8.pack dopShErr1 -- Rec. 2, Col. 48-58
			putByteString $ Ch8.pack dopShErr2 -- Rec. 2, Col. 59-62
	      )
	      dopplerShiftErrHz
	putByteString $ Ch8.pack sigFreq3	-- Rec. 2, Col. 63-68
	putByteString $ Ch8.pack $ observatoryGetCode sigSrcObservatory -- 69-71
	putWord8  32 -- ' '			-- Rec. 2, Col. 72
	putByteString $ Ch8.pack obsRfcCode	-- Rec. 2, Col. 73-77
	putByteString $ Ch8.pack $ observatoryGetCode observatory -- R2,Col78-80
	putWord8 10 -- Terminating line-feed character.
putRecObserv (Rec { objNumber
		  , provDesign
		  , discovery
		  , note1
		  , j2000Adj
		  , time
		  , observer = SatelliteObs {..}
		  , observatory }) = do
	putObsData obsData
	putByteString $ Ch8.pack $ observatoryGetCode observatory -- Col. 78-80
	putWord8 10 -- Terminating line-feed character.
	let (ObsData {..}) = obsData
        if objNumber > 0 then putAlNumWithWidth 5 objNumber
			 else putByteString $ Ch8.pack "     "
	putByteString $ Ch8.pack $		-- Rec. 2, Col.  6-12
		maybe "       " (\d -> PD.showPacked d) provDesign
	putWord8  32 -- ' '			-- Rec. 2, Col. 13
	putWord8 $ fromIntegral $ fromEnum note1 --Rec. 2, Col. 14
	putWord8 115 -- 's'			-- Rec. 2, Col. 15
	putTime time				-- Rec. 2, Col. 16-32
	putWord8 $ fromIntegral $ fromEnum unitsToUse	-- Rec. 2, Col. 33
	putWord8  32 -- ' '			-- Rec. 2, Col. 34
	let unitF = fromSIFactor unitsToUse
	let (parallaxX, parallaxY, parallaxZ) = parallaxM
	putByteString $ Ch8.pack $ printf "%+011.4f" $ unitF*parallaxX --2,35-45
	putWord8  32 -- ' '			-- Rec. 2, Col. 46
	putByteString $ Ch8.pack $ printf "%+011.4f" $ unitF*parallaxY --2,47-57
	putWord8  32 -- ' '			-- Rec. 2, Col. 58
	putByteString $ Ch8.pack $ printf "%+011.4f" $ unitF*parallaxZ --2,59-69
	putByteString $ Ch8.pack "   "		-- Rec. 2, Col. 70-72
	putByteString $ Ch8.pack rfcCode	-- Rec. 2, Col. 73-77
	putByteString $ Ch8.pack $ observatoryGetCode observatory -- R.2,C.78-80
	putWord8 10 -- Terminating line-feed character.
putRecObserv (Rec {..}) = do
	putObsData $ obsData observer
	putByteString $ Ch8.pack $ observatoryGetCode observatory -- Col. 78-80
	putWord8 10 -- Terminating line-feed character.


getRecObserv  82 = do	-- 'R'
	sigTimeDelayMuS <- mayGetFloatImplFPWithWidth 11 4 -- Col. 33-47
	dopplerShiftHz <- mayGetFloatImplFPWithWidth 11 4  -- Col. 48-62
	sigFreqMHzPt1 <- getFloatImplFPWithWidth 5 1	-- Col. 63-68
	sigSrcObsCode <- getTrimmedStrWithWidth 3	-- Col. 69-71
	let sigSrcObservatory = ObservatoryCode { obsCode=sigSrcObsCode }
	_ <- getWord8					-- Col. 72
	obsRfcCode <- getStrWithWidth 5			-- Col. 73-77
	obsCode <- getTrimmedStrWithWidth 3		-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	skip 32					-- Rec. 2, Col.  1-32
	sigReturnPt <- getEnumWord8		-- Rec. 2, Col. 33
	sigTimeDelayErrMuS <- mayGetFloatImplFPWithWidth 10 4 -- Rec.2,Col.34-47
	dopplerShiftErrHz <- mayGetFloatImplFPWithWidth 11 4  -- Rec.2,Col.48-62
	sigFreqMHz <- getDecFracWithWidth' sigFreqMHzPt1 0.001 6 --Rec2,Col63-68
	let sigFreqHz = 1000000 * sigFreqMHz
	skip 12					-- Rec. 2, Col. 69-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( RadarObs {..}, ObservatoryCode {..} )
getRecObserv  83 = do	-- 'S'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	skip 32					-- Rec. 2, Col.  1-32
	unitsToUse <- getEnumWord8		-- Rec. 2, Col. 33
	_ <- getWord8				-- Rec. 2, Col. 34
  	parallaxX <- getFloatWithWidth 11	-- Rec. 2, Col. 35-45
	_ <- getWord8				-- Rec. 2, Col. 46
	parallaxY <- getFloatWithWidth 11	-- Rec. 2, Col. 47-57
	_ <- getWord8				-- Rec. 2, Col. 58
	parallaxZ <- getFloatWithWidth 11	-- Rec. 2, Col. 59-69
	let unitF = toSIFactor unitsToUse
	let parallaxM = (unitF*parallaxX, unitF*parallaxY, unitF*parallaxZ)
	skip 11					-- Rec. 2, Col. 70-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( SatelliteObs {..}, ObservatoryCode {..} )
getRecObserv  65 = do	-- 'A'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (OpticalObs {..}, ObservatoryCode {..})
getRecObserv  32 = do	-- ' ' (default)
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (PhotographicObs {..}, ObservatoryCode {..})
getRecObserv  80 = do	-- 'P'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (PhotographicObs {..}, ObservatoryCode {..})
getRecObserv 101 = do	-- 'e'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (EncoderObs {..}, ObservatoryCode {..})
getRecObserv  67 = do	-- 'C'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (CCDObs {..}, ObservatoryCode {..})
getRecObserv  84 = do	-- 'T'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (MeridianOrTransitCircObs {..}, ObservatoryCode {..})
getRecObserv  77 = do	-- 'M'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (MicrometerObs {..}, ObservatoryCode {..})
getRecObserv  86 = do	-- 'V'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	skip 34					-- Rec. 2, Col.  1-34
	lonDeg <- getFloatWithWidth 10		-- Rec. 2, Col. 35-44
	_ <- getWord8				-- Rec. 2, Col. 45
	latDeg <- getFloatWithWidth 10		-- Rec. 2, Col. 46-55
	_ <- getWord8				-- Rec. 2, Col. 56
	altMeters <- readWithWidth 5		-- Rec. 2, Col. 57-61
	skip 19					-- Rec. 2, Col. 62-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( RovingObserverObs {..},
		 RovingObserver { lonRad = lonDeg * pi / 180
				, latRad = latDeg * pi / 180
				, altMeters } )
getRecObserv  99 = do	-- 'c'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( CorrectedWithoutRepubCCDObs {..}, ObservatoryCode {..} )
getRecObserv  69 = do	-- 'E'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( OccultationDerivObs {..}, ObservatoryCode {..} )
getRecObserv  79 = do	-- 'O'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( OffsetObs {..}, ObservatoryCode {..} )
getRecObserv  72 = do	-- 'H'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( HipparcosObs {..}, ObservatoryCode {..} )
getRecObserv  78 = do	-- 'N'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( NormalPlaceObs {..}, ObservatoryCode {..} )
getRecObserv 110 = do	-- 'n'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( VideoFrmAvgObs {..}, ObservatoryCode {..} )
getRecObserv  88 = do	-- 'X'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( ReplacedObs {obsData, lowercaseX=False}, ObservatoryCode {..} )
getRecObserv 120 = do	-- 'x'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( ReplacedObs {obsData, lowercaseX=True }, ObservatoryCode {..} )


putRec rec@(Rec {..}) = do
	let (ObsData {..}) = obsData observer
        if objNumber > 0 then putAlNumWithWidth 5 objNumber
			 else putByteString $ Ch8.pack "     "
	putByteString $ Ch8.pack $		-- Col.  6-12
		maybe "       " (\d -> PD.showPacked d) provDesign
	putWord8 $ if discovery then 42 else 32 -- '*' or ' '	Col. 13
	putWord8 $ fromIntegral $ fromEnum note1 --Col. 14
	putWord8 $ obsTypeNum observer		-- Col. 15
	putTime time				-- Col. 16-32
	putRecObserv rec

getRec = do
	objNumber <- getAlNumWithWidthDeflt 5 (-1)	-- Col.  1- 5
	provDesign <- PD.mayGetPacked		-- Col.  6-12
	discoveryBs <- getWord8			-- Col. 13
	let discovery = discoveryBs == 42
	note1 <- getEnumWord8			-- Col. 14
	note2W8 <- getWord8			-- Col. 15
	let j2000Adj = note2W8GetJ2000Adj note2W8
	time <- getTime				-- Col. 16-32
	(observer, observatory) <- getRecObserv note2W8
	return Rec {..}


readSubRecs32b expectedNote2 = do
	objNumber <- getAlNumWithWidthDeflt 5 (-1)	-- Rec. 2, Col.  1- 5
	provDesign <- PD.mayGetPacked		-- Rec. 2, Col.  6-12
	discoveryBs <- getWord8			-- Rec. 2, Col. 13
	let discovery = discoveryBs == 42
	note1 <- getEnumWord8			-- Rec. 2, Col. 14
	note2W8 <- getWord8			-- Rec. 2, Col. 15
	let j2000Adj = note2W8GetJ2000Adj note2W8
	time <- getTime				-- Rec. 2, Col. 16-32
	if note2W8 == expectedNote2
		then return []
		else do (observer, observatory, r1) <- getRecChunk2 note2W8
			r2 <- readSubRecs32b expectedNote2
			return $ (Rec {..}):(r1 ++ r2)

getRecChunk2 :: Word8 -> Get (Observer, Observatory, [Rec])
getRecChunk2  82 = do	-- 'R'
	sigTimeDelayMuS <- mayGetFloatImplFPWithWidth 11 4 -- Col. 33-47
	dopplerShiftHz <- mayGetFloatImplFPWithWidth 11 4  -- Col. 48-62
	sigFreqMHzPt1 <- getFloatImplFPWithWidth 5 1	-- Col. 63-68
	sigSrcObsCode <- getTrimmedStrWithWidth 3	-- Col. 69-71
	let sigSrcObservatory = ObservatoryCode { obsCode = sigSrcObsCode }
	_ <- getWord8					-- Col. 72
	obsRfcCode <- getStrWithWidth 5			-- Col. 73-77
	obsCode <- getTrimmedStrWithWidth 3		-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	subRecs <- readSubRecs32b 114			-- Rec. 2, Col. 1-32
	sigReturnPt <- getEnumWord8		-- Rec. 2, Col. 33
	sigTimeDelayErrMuS <- mayGetFloatImplFPWithWidth 10 4 -- Rec.2,Col.34-47
	dopplerShiftErrHz <- mayGetFloatImplFPWithWidth 11 4  -- Rec.2,Col.48-62
	sigFreqMHz <- getDecFracWithWidth' sigFreqMHzPt1 0.001 6 --Rec2,Col63-68
	let sigFreqHz = 1000000 * sigFreqMHz
	skip 12					-- Rec. 2, Col. 69-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( RadarObs {..}, ObservatoryCode {..}, subRecs )
getRecChunk2  83 = do	-- 'S'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	subRecs <- readSubRecs32b 115		-- Rec. 2, Col.  1-32
	unitsToUse <- getEnumWord8		-- Rec. 2, Col. 33
	_ <- getWord8				-- Rec. 2, Col. 34
  	parallaxX <- getFloatWithWidth 11	-- Rec. 2, Col. 35-45
	_ <- getWord8				-- Rec. 2, Col. 46
	parallaxY <- getFloatWithWidth 11	-- Rec. 2, Col. 47-57
	_ <- getWord8				-- Rec. 2, Col. 58
	parallaxZ <- getFloatWithWidth 11	-- Rec. 2, Col. 59-69
	let unitF = toSIFactor unitsToUse
	let parallaxM = (unitF*parallaxX, unitF*parallaxY, unitF*parallaxZ)
	skip 11					-- Rec. 2, Col. 70-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( SatelliteObs {..}, ObservatoryCode {..}, subRecs )
getRecChunk2  86 = do	-- 'V'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	subRecs <- readSubRecs32b 118		-- Rec. 2, Col.  1-32
	skip 2					-- Rec. 2, Col. 33-34
	--skip 34					-- Rec. 2, Col.  1-34
	lonDeg <- getFloatWithWidth 10		-- Rec. 2, Col. 35-44
	let lonRad = lonDeg * pi / 180
	_ <- getWord8				-- Rec. 2, Col. 45
	latDeg <- getFloatWithWidth 10		-- Rec. 2, Col. 46-55
	let latRad = latDeg * pi / 180
	_ <- getWord8				-- Rec. 2, Col. 56
	altMeters <- readWithWidth 5		-- Rec. 2, Col. 57-61
	skip 19					-- Rec. 2, Col. 62-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( RovingObserverObs {..}, RovingObserver { .. }, subRecs )
getRecChunk2  65 = do	-- 'A'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( OpticalObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  32 = do	-- ' ' (default)
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( PhotographicObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  80 = do	-- 'P'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( PhotographicObs {..}, ObservatoryCode {..}, [] )
getRecChunk2 101 = do	-- 'e'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( EncoderObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  67 = do	-- 'C'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( CCDObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  84 = do	-- 'T'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( MeridianOrTransitCircObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  77 = do	-- 'M'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( MicrometerObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  99 = do	-- 'c'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( CorrectedWithoutRepubCCDObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  69 = do	-- 'E'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( OccultationDerivObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  79 = do	-- 'O'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( OffsetObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  72 = do	-- 'H'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( HipparcosObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  78 = do	-- 'N'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( NormalPlaceObs {..}, ObservatoryCode {..}, [] )
getRecChunk2 110 = do	-- 'n'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return ( VideoFrmAvgObs {..}, ObservatoryCode {..}, [] )
getRecChunk2  88 = do	-- 'X'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (ReplacedObs {obsData,lowercaseX=False}, ObservatoryCode {..},[])
getRecChunk2 120 = do	-- 'x'
	obsData <- getObsData
	obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (ReplacedObs {obsData,lowercaseX=True }, ObservatoryCode {..},[])
getRecChunk2 x = error $ "Unrecognized note2 value: " ++ (show x)

getRecChunk = do
	objNumber <- getAlNumWithWidthDeflt 5 (-1)	-- Col.  1- 5
	provDesign <- PD.mayGetPacked		-- Col.  6-12
	discoveryBs <- getWord8			-- Col. 13
	let discovery = discoveryBs == 42
	note1 <- getEnumWord8			-- Col. 14
	note2W8 <- getWord8			-- Col. 15
	let j2000Adj = note2W8GetJ2000Adj note2W8
	time <- getTime				-- Col. 16-32
	(observer, observatory, restRecs) <- getRecChunk2 note2W8
	return $ (Rec {..}):restRecs


instance Binary Rec where
  get = getRec
  put = putRec


{-
getRecs :: LBS.ByteString -> [Rec]
getRecs bs =
	if LBS.null bs	then []
			else let (rec, restBs, ofs) = runGetState getRec bs 0
			     in rec:(getRecs restBs)
-}

getRecs :: LBS.ByteString -> [Rec]
getRecs bs =
	if LBS.null bs
	  then []
	  else let (recs, restBs, ofs) = runGetState getRecChunk bs 0
	       in recs ++ (getRecs restBs)

{-
getMayRecs :: LBS.ByteString -> [Rec]
getMayRecs bs =
	case LBS.null bs of
	  True -> []
	  _    -> let (rec, recTail) = LBS.splitAt chunkSize bs
	  	      recsTail = getMayRecs recTail
	          in maybe recsTail
		  	   (\rec -> rec:(recsTail))
			   (runGet mayGetRec bs)
-}
