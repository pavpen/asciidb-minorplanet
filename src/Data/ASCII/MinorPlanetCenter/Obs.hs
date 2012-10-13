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
  , Annot (..)
  , chunkSize
  , note2Ofs
  , recGetChunkCnt
  , putRec
  , getRec
  , getRecs
  , getRecChunk
  , getAnnotRecs
  , getAnnotRecChunk
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


-- | An Minor Planet Center observation record:
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

instance Binary Rec where
  get = getRec
  put = putRec


-- | Record annotation:
data Annot = Annot
	   { srcBytes :: LBS.ByteString -- ^ source bytes for the record
	   , lineNums :: [Int]	  -- ^ line numbers for the source bytes
	   } deriving (Eq, Show)


chunkSize = 81
note2Ofs = 14

recGetChunkCnt (Rec {observer = RovingObserverObs {..}}) = 2
recGetChunkCnt (Rec {observer = RadarObs {..}}) = 2
recGetChunkCnt (Rec {observer = SatelliteObs {..}}) = 2
recGetChunkCnt _ = 1


-- | Read columns 1-32 of an observation record.
getC1_32 = do
	objNumber <- getAlNumWithWidthDeflt 5 (-1)	-- Col.  1- 5
	provDesign <- PD.mayGetPacked			-- Col.  6-12
	discoveryBs <- getWord8				-- Col. 13
	let discovery = discoveryBs == 42
	note1 <- getEnumWord8				-- Col. 14
	note2W8 <- getWord8				-- Col. 15
	let j2000Adj = note2W8GetJ2000Adj note2W8
	time <- getTime					-- Col. 16-32
	return (objNumber,provDesign,discovery,note1,note2W8,j2000Adj,time)

-- | Read columns 33-80 of optical observations.
getOptC33_80 = do
	obsData <- getObsData				-- Col. 33--77
	obsCode <- getTrimmedStrWithWidth 3		-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (obsData, obsCode)

-- | Read columns 33-80 of radar observations.
getRadarC33_80 = do
	sigTimeDelayMuS <- mayGetFloatImplFPWithWidth 11 4 -- Col. 33-47
	dopplerShiftHz <- mayGetFloatImplFPWithWidth 11 4  -- Col. 48-62
	sigFreqMHzPt1 <- getFloatImplFPWithWidth 5 1	-- Col. 63-68
	sigSrcObsCode <- getTrimmedStrWithWidth 3	-- Col. 69-71
	let sigSrcObservatory = ObservatoryCode { obsCode = sigSrcObsCode }
	_ <- getWord8					-- Col. 72
	obsRfcCode <- getStrWithWidth 5			-- Col. 73-77
	obsCode <- getTrimmedStrWithWidth 3		-- Col. 78-80
	uncheckedSkip 1 -- Skip the terminating '\n'.
	return (sigTimeDelayMuS, dopplerShiftHz, sigFreqMHzPt1,
		sigSrcObservatory, obsRfcCode, obsCode)


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



-- | Get the next chunk of records (in the Get monad).  (The 'getRec' method
-- only works on files with no nested records.)
--
--   Most of the time a chunk of records is just a singleton list of one
--   record.  However, if records are nested, the chunk will be a flat list of
--   the container record, followed by all of the records nested in it.
--
--   Note: I did not find record nesting documented on the Minor Planet
--   Center's website; it was introducted to deal with samples from
--   <NumObs.txt.gz> such as this:
--
-- 38071J99G03U  R1999 04 17.381944               -    461281     8560 253 41081253
-- 38071         R1999 04 17.381944               -    46128100   8560 253 JPLRS253
-- 38071         r1999 04 17.381944C                       30000       253 JPLRS253
-- 38071J99G03U  r1999 04 17.381944C                       30          253 41081253
-- 38071J99G03U  R1999 04 17.583333               -    48354690   8560 253 41081253
-- 38071         R1999 04 17.583333               -    48354690   8560 253 JPLRS253
-- 38071         r1999 04 17.583333C                        0200       253 JPLRS253
-- 38071J99G03U  r1999 04 17.583333C                        020        253 41081253
--   
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


readAnnotSubRecs32b lnNum expectedNote2 = do
	bs32 <- getLazyByteString 32
	return $ runGet (act bs32) bs32
  where act bs32 = do
	  objNumber <- getAlNumWithWidthDeflt 5 (-1)	-- Rec. 2, Col.  1- 5
	  provDesign <- PD.mayGetPacked		-- Rec. 2, Col.  6-12
	  discoveryBs <- getWord8		-- Rec. 2, Col. 13
	  let discovery = discoveryBs == 42
	  note1 <- getEnumWord8			-- Rec. 2, Col. 14
	  note2W8 <- getWord8			-- Rec. 2, Col. 15
	  let j2000Adj = note2W8GetJ2000Adj note2W8
	  time <- getTime			-- Rec. 2, Col. 16-32
	  if note2W8 == expectedNote2
	   then return ([], lnNum, bs32)
	   else do (observer, observatory, annot@(Annot {..}), r1) <-
	   		getAnnotRecChunk2 lnNum note2W8
		   (r2, lnNum', bs32') <-
		   	readAnnotSubRecs32b ((last lineNums) + 1) expectedNote2
		   return $ ( (Rec {..},
		   		 annot {srcBytes = bs32 `LBS.append` srcBytes}):
			       (r1 ++ r2),
			      lnNum', bs32')

getAnnotRecChunk2 :: Int -> Word8
		-> Get (Observer, Observatory, Annot, [(Rec, Annot)])
getAnnotRecChunk2 lnNum  82 = do	-- 'R'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  sigTimeDelayMuS <- mayGetFloatImplFPWithWidth 11 4 -- Col. 33-47
	  dopplerShiftHz <- mayGetFloatImplFPWithWidth 11 4  -- Col. 48-62
	  sigFreqMHzPt1 <- getFloatImplFPWithWidth 5 1	-- Col. 63-68
	  sigSrcObsCode <- getTrimmedStrWithWidth 3	-- Col. 69-71
	  let sigSrcObservatory = ObservatoryCode { obsCode = sigSrcObsCode }
	  _ <- getWord8					-- Col. 72
	  obsRfcCode <- getStrWithWidth 5			-- Col. 73-77
	  obsCode <- getTrimmedStrWithWidth 3		-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  						-- Rec. 2, Col. 1-32
	  (subRecs, lnNum', bs2_32) <- readAnnotSubRecs32b (lnNum + 1) 114
	  bs2_49 <- getLazyByteString 49
	  return $ runGet ( act' (bs49 `LBS.append` bs2_32 `LBS.append` bs2_49)
	  		    lnNum' subRecs sigTimeDelayMuS dopplerShiftHz
			    sigFreqMHzPt1 sigSrcObsCode obsCode)
		   bs2_49
	act' bs130 lnNum' subRecs sigTimeDelayMuS dopplerShiftHz sigFreqMHzPt1 sigSrcObsCode obsCode = do
	  sigReturnPt <- getEnumWord8			-- Rec. 2, Col. 33
	  sigTimeDelayErrMuS <- mayGetFloatImplFPWithWidth 10 4 -- R.2,Col.34-47
	  dopplerShiftErrHz <- mayGetFloatImplFPWithWidth 11 4  -- R.2,Col.48-62
	  sigFreqMHz <- getDecFracWithWidth' sigFreqMHzPt1 0.001 6 --R2,Col63-68
	  let sigFreqHz = 1000000 * sigFreqMHz
	  skip 12					-- Rec. 2, Col. 69-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs130
			    , lineNums = [lnNum, lnNum'] }
	  return ( RadarObs {..}, ObservatoryCode {..}, annot, subRecs )
getAnnotRecChunk2 lnNum  83 = do	-- 'S'
	bs49 <- getLazyByteString 49
	let (obsData, obsCode) = runGet getOptC33_80 bs49
	  						-- Rec. 2, Col.  1-32
	(subRecs, lnNum', bs2_32) <- readAnnotSubRecs32b (lnNum + 1) 115
	bs2_49 <- getLazyByteString 49
	return $ runGet ( act' (bs49 `LBS.append` bs2_32 `LBS.append` bs2_49)
	  		  lnNum' subRecs obsData obsCode )
		 bs2_49
  where	act' bs130 lnNum' subRecs obsData obsCode = do
	  unitsToUse <- getEnumWord8			-- Rec. 2, Col. 33
	  _ <- getWord8					-- Rec. 2, Col. 34
  	  parallaxX <- getFloatWithWidth 11		-- Rec. 2, Col. 35-45
	  _ <- getWord8					-- Rec. 2, Col. 46
	  parallaxY <- getFloatWithWidth 11		-- Rec. 2, Col. 47-57
	  _ <- getWord8					-- Rec. 2, Col. 58
	  parallaxZ <- getFloatWithWidth 11		-- Rec. 2, Col. 59-69
	  let unitF = toSIFactor unitsToUse
	  let parallaxM = (unitF*parallaxX, unitF*parallaxY, unitF*parallaxZ)
	  skip 11					-- Rec. 2, Col. 70-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs130
			    , lineNums = [lnNum, lnNum'] }
	  return ( SatelliteObs {..}, ObservatoryCode {..}, annot, subRecs )
getAnnotRecChunk2 lnNum  86 = do	-- 'V'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3		-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
							-- Rec. 2, Col. 1-32:
	  (subRecs, lnNum', bs2_32) <- readAnnotSubRecs32b (lnNum + 1) 118
	  bs2_49 <- getLazyByteString 49
	  return $ runGet ( act' (bs49 `LBS.append` bs2_32 `LBS.append` bs2_49)
	  		    lnNum' subRecs obsData obsCode )
		   bs2_49
	act' bs130 lnNum' subRecs obsData obsCode = do
	  skip 2					-- Rec. 2, Col. 33-34
	  lonDeg <- getFloatWithWidth 10		-- Rec. 2, Col. 35-44
	  let lonRad = lonDeg * pi / 180
	  _ <- getWord8					-- Rec. 2, Col. 45
	  latDeg <- getFloatWithWidth 10		-- Rec. 2, Col. 46-55
	  let latRad = latDeg * pi / 180
	  _ <- getWord8					-- Rec. 2, Col. 56
	  altMeters <- readWithWidth 5			-- Rec. 2, Col. 57-61
	  skip 19					-- Rec. 2, Col. 62-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs130
			    , lineNums = [lnNum, lnNum'] }
	  return (RovingObserverObs {..}, RovingObserver { .. }, annot, subRecs)
getAnnotRecChunk2 lnNum  65 = do	-- 'A'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( OpticalObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  32 = do	-- ' ' (default)
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( PhotographicObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  80 = do	-- 'P'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( PhotographicObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum 101 = do	-- 'e'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( EncoderObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  67 = do	-- 'C'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( CCDObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  84 = do	-- 'T'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return (MeridianOrTransitCircObs {..}, ObservatoryCode {..}, annot,[])
getAnnotRecChunk2 lnNum  77 = do	-- 'M'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( MicrometerObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  99 = do	-- 'c'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( CorrectedWithoutRepubCCDObs {..}, ObservatoryCode {..},
	  	   annot, [] )
getAnnotRecChunk2 lnNum  69 = do	-- 'E'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( OccultationDerivObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  79 = do	-- 'O'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( OffsetObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  72 = do	-- 'H'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( HipparcosObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  78 = do	-- 'N'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( NormalPlaceObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum 110 = do	-- 'n'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return ( VideoFrmAvgObs {..}, ObservatoryCode {..}, annot, [] )
getAnnotRecChunk2 lnNum  88 = do	-- 'X'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return (ReplacedObs {obsData,lowercaseX=False}, ObservatoryCode {..},
	  	  annot, [])
getAnnotRecChunk2 lnNum 120 = do	-- 'x'
	bs49 <- getLazyByteString 49
	return $ runGet (act bs49) bs49
  where act bs49 = do
	  obsData <- getObsData
	  obsCode <- getTrimmedStrWithWidth 3	-- Col. 78-80
	  uncheckedSkip 1 -- Skip the terminating '\n'.
	  let annot = Annot { srcBytes = bs49
			    , lineNums = [lnNum] }
	  return (ReplacedObs {obsData,lowercaseX=True }, ObservatoryCode {..},
		  annot, [])
getAnnotRecChunk2 lnNum x = error $ "Unrecognized note2 value: " ++ (show x)



-- | Like 'getRecChunk', but get each records with an annotation ('Annot')
--   associated with it.
getAnnotRecChunk lnNum = do
	bs32 <- getLazyByteString 32
	let (objNumber, provDesign, discovery, note1, note2W8, j2000Adj, time) =
		runGet getC1_32 bs32
	(observer, observatory, annot@(Annot {srcBytes}), restRecs) <-
	  	getAnnotRecChunk2 lnNum note2W8
	return $ (Rec {..}, annot { srcBytes = bs32 `LBS.append` srcBytes }):
	  	   restRecs



getRecs :: LBS.ByteString -> [Rec]
getRecs bs =
	if LBS.null bs
	  then []
	  else let (recs, restBs, ofs) = runGetState getRecChunk bs 0
	       in recs ++ (getRecs restBs)

getAnnotRecs :: Int -> LBS.ByteString -> [(Rec, Annot)]
getAnnotRecs lnNum bs
  | LBS.null bs = []
  | otherwise   =
	let (recs, restBs, ofs) = runGetState (getAnnotRecChunk lnNum) bs 0
	    (_, Annot { lineNums }) = head recs
  	    lnNum' = 1 + (last $ lineNums)
	in recs ++ (getAnnotRecs lnNum' restBs)

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
