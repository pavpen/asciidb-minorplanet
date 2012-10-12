{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings, QuasiQuotes,
    RecordWildCards, TemplateHaskell, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.Persist.MinorPlanetCenter.DbTypes
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- 'Persistent' datatypes and mapping to the 'Obs.Rec' datatype, and those of
-- its fields.  (I.e. this is the bridge to, e.g. SQL, database backends via
-- the 'groundhog' package.)
--
-----------------------------------------------------------------------------

module Database.Persist.MinorPlanetCenter.DbTypes where

import Data.ASCII.MinorPlanetCenter.Note2
					(readNote2W8)
import Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Time			(UTCTime)
import Data.Word			(Word8)
import Database.Groundhog.TH		(CodegenConfig (..),
					 defaultCodegenConfig,
					 persistentNamingStyle, mkPersist,
					 groundhog)
import Database.Groundhog.Sqlite	()


data DbObsData	= DbObsData
		{ rightAscSec :: Double
		, declRad :: Double
		, magnitude :: Maybe Double
		, dbObsBand :: Int
		, rfcCode :: String
		} deriving (Eq, Show)

fromDbObsData (DbObsData {..}) =	let obsBand = toEnum dbObsBand
					in MPCObs.ObsData {..}
toDbObsData (MPCObs.ObsData {..}) =	let dbObsBand = fromEnum obsBand
					in DbObsData {..}


data DbObserver	= DbOpticalObs		{ dbObsData :: DbObsData }
		| DbPhotographicObs	{ dbObsData :: DbObsData }
		| DbEncoderObs		{ dbObsData :: DbObsData }
		| DbCCDObs		{ dbObsData :: DbObsData }
		| DbMeridianOrTransitCircObs
					{ dbObsData :: DbObsData }
		| DbMicrometerObs	{ dbObsData :: DbObsData }
		| DbRovingObserverObs	{ dbObsData :: DbObsData }
		| DbRadarObs		{ sigSrcObservatory ::MPCObs.Observatory
					, sigTimeDelayMuS :: Maybe Double
					, sigTimeDelayErrMuS :: Maybe Double
					, dopplerShiftHz :: Maybe Double
					, dopplerShiftErrHz :: Maybe Double
					, sigFreqHz :: Double
					, dbSigReturnPt :: Int
					, obsRfcCode :: String
					}
		| DbSatelliteObs	{ dbObsData :: DbObsData
					, parallaxM :: (Double, Double, Double)
					, dbUnitsToUse :: Int
					}
		| DbCorrectedWithoutRepubCCDObs
					{ dbObsData :: DbObsData }
		| DbOccultationDerivObs	{ dbObsData :: DbObsData }
		| DbOffsetObs		{ dbObsData :: DbObsData }
		| DbHipparcosObs	{ dbObsData :: DbObsData }
		| DbNormalPlaceObs	{ dbObsData :: DbObsData }
		| DbVideoFrmAvgObs	{ dbObsData :: DbObsData }
		| DbReplacedObs		{ dbObsData :: DbObsData
					, lowercaseX :: Bool }
		deriving (Eq, Show)

toDbObserver (MPCObs.OpticalObs {..}) = let dbObsData = toDbObsData obsData
					in DbOpticalObs {..}
toDbObserver (MPCObs.PhotographicObs {..}) = let dbObsData = toDbObsData obsData
					     in DbPhotographicObs {..}
toDbObserver (MPCObs.EncoderObs {..}) = let dbObsData = toDbObsData obsData
					in DbEncoderObs {..}
toDbObserver (MPCObs.CCDObs {..}) = let dbObsData = toDbObsData obsData
				    in DbCCDObs {..}
toDbObserver (MPCObs.MeridianOrTransitCircObs {..}) =
	let dbObsData = toDbObsData obsData
	in DbMeridianOrTransitCircObs {..}
toDbObserver (MPCObs.MicrometerObs {..}) = let dbObsData = toDbObsData obsData
					   in DbMicrometerObs {..}
toDbObserver (MPCObs.RovingObserverObs {..}) =
	let dbObsData = toDbObsData obsData
	in DbRovingObserverObs {..}
toDbObserver (MPCObs.RadarObs {..}) = let dbSigReturnPt = fromEnum sigReturnPt
				      in DbRadarObs {..}
toDbObserver (MPCObs.SatelliteObs {..}) = let dbObsData = toDbObsData obsData
					      dbUnitsToUse = fromEnum unitsToUse
					  in DbSatelliteObs {..}
toDbObserver (MPCObs.CorrectedWithoutRepubCCDObs {..}) =
	let dbObsData = toDbObsData obsData
	in DbCorrectedWithoutRepubCCDObs {..}
toDbObserver (MPCObs.OccultationDerivObs {..}) =
	let dbObsData = toDbObsData obsData
	in DbOccultationDerivObs {..}
toDbObserver (MPCObs.OffsetObs {..}) =
	let dbObsData = toDbObsData obsData
	in DbOffsetObs {..}
toDbObserver (MPCObs.HipparcosObs {..}) = let dbObsData = toDbObsData obsData
					  in DbHipparcosObs {..}
toDbObserver (MPCObs.NormalPlaceObs {..}) = let dbObsData = toDbObsData obsData
					    in DbNormalPlaceObs {..}
toDbObserver (MPCObs.VideoFrmAvgObs {..}) = let dbObsData = toDbObsData obsData
					    in DbVideoFrmAvgObs {..}
toDbObserver (MPCObs.ReplacedObs {..}) = let dbObsData = toDbObsData obsData
					 in DbReplacedObs {..}

fromDbObserver (DbOpticalObs {..}) = let obsData = fromDbObsData dbObsData
				     in MPCObs.OpticalObs {..}
fromDbObserver (DbPhotographicObs {..}) = let obsData = fromDbObsData dbObsData
					  in MPCObs.PhotographicObs {..}
fromDbObserver (DbEncoderObs {..}) = let obsData = fromDbObsData dbObsData
				     in MPCObs.EncoderObs {..}
fromDbObserver (DbCCDObs {..}) = let obsData = fromDbObsData dbObsData
				 in MPCObs.CCDObs {..}
fromDbObserver (DbMeridianOrTransitCircObs {..}) =
	let obsData = fromDbObsData dbObsData
	in MPCObs.MeridianOrTransitCircObs {..}
fromDbObserver (DbMicrometerObs {..}) = let obsData = fromDbObsData dbObsData
					in MPCObs.MicrometerObs {..}
fromDbObserver (DbRovingObserverObs {..}) =
	let obsData = fromDbObsData dbObsData
	in MPCObs.RovingObserverObs {..}
fromDbObserver (DbRadarObs {..}) = let sigReturnPt = toEnum dbSigReturnPt
				   in MPCObs.RadarObs {..}
fromDbObserver (DbSatelliteObs {..}) = let obsData = fromDbObsData dbObsData
					   unitsToUse = toEnum dbUnitsToUse
				       in MPCObs.SatelliteObs {..}
fromDbObserver (DbCorrectedWithoutRepubCCDObs {..}) =
	let obsData = fromDbObsData dbObsData
	in MPCObs.CorrectedWithoutRepubCCDObs {..}
fromDbObserver (DbOccultationDerivObs {..}) =
	let obsData = fromDbObsData dbObsData
	in MPCObs.OccultationDerivObs {..}
fromDbObserver (DbOffsetObs {..}) = let obsData = fromDbObsData dbObsData
				    in MPCObs.OffsetObs {..}
fromDbObserver (DbHipparcosObs {..}) = let obsData = fromDbObsData dbObsData
				       in MPCObs.HipparcosObs {..}
fromDbObserver (DbNormalPlaceObs {..}) = let obsData = fromDbObsData dbObsData
					 in MPCObs.NormalPlaceObs {..}
fromDbObserver (DbVideoFrmAvgObs {..}) = let obsData = fromDbObsData dbObsData
					 in MPCObs.VideoFrmAvgObs {..}
fromDbObserver (DbReplacedObs {..}) = let obsData = fromDbObsData dbObsData
				      in MPCObs.ReplacedObs {..}


{-
data DbProvDesign = DbMinorPlanetId { year :: Int
				    , month :: Int
				    , monthSecHalf :: Bool
				    , order :: Int }
		  | DbSurveyPLId    { order :: Int }
		  | DbSurveyT1Id    { order :: Int }
		  | DbSurveyT2Id    { order :: Int }
		  | DbSurveyT3Id    { order :: Int }
		  | DbCometId	    { year :: Int
		  		    , month :: Int
				    , monthSecHalf :: Bool
				    , order :: Int
				    , dbStatus :: Int
				    , dbFragment :: Int }
		  deriving (Eq, Show)

toDbProvDesign (MPCObs.MinorPlanetId {..}) = DbMinorPlanetId {..}
toDbProvDesign (MPCObs.SurveyPLId {..}) = DbSurveyPLId {..}
toDbProvDesign (MPCObs.SurveyT1Id {..}) = DbSurveyT1Id {..}
toDbProvDesign (MPCObs.SurveyT2Id {..}) = DbSurveyT2Id {..}
toDbProvDesign (MPCObs.SurveyT3Id {..}) = DbSurveyT3Id {..}
toDbProvDesign (MPCObs.CometId {..}) = let dbStatus = fromEnum status
					   dbFragment = fromEnum fragment
				       in DbCometId {..}

fromDbProvDesign (DbMinorPlanetId {..}) = MPCObs.MinorPlanetId {..}
fromDbProvDesign (DbSurveyPLId {..}) = MPCObs.SurveyPLId {..}
fromDbProvDesign (DbSurveyT1Id {..}) = MPCObs.SurveyT1Id {..}
fromDbProvDesign (DbSurveyT2Id {..}) = MPCObs.SurveyT2Id {..}
fromDbProvDesign (DbSurveyT3Id {..}) = MPCObs.SurveyT3Id {..}
fromDbProvDesign (DbCometId {..}) = let status = toEnum dbStatus
					fragment = toEnum dbFragment
				    in MPCObs.CometId {..}
-}

type DbProvDesign = String

toDbProvDesign = PD.showPacked
fromDbProvDesign = PD.readPacked


data DbRec = DbRec
	   { objNumber :: Int
	   , dbProvDesign :: Maybe DbProvDesign
	   , discovery :: Bool
	   , dbNote1 :: Int
	   , j2000Adj :: Bool
	   , time :: UTCTime
	   , dbObserver :: DbObserver
	   , observatory :: MPCObs.Observatory
	   } deriving (Eq, Show)

toDbRec (MPCObs.Rec {..}) = let dbProvDesign = fmap toDbProvDesign provDesign
				dbNote1 = fromEnum note1
				dbObserver = toDbObserver observer
			    in DbRec {..}

fromDbRec (DbRec {..}) = let provDesign = fmap fromDbProvDesign dbProvDesign
			     note1 = toEnum dbNote1
			     observer = fromDbObserver dbObserver
			 in MPCObs.Rec {..}


mkPersist (defaultCodegenConfig { namingStyle = persistentNamingStyle
				, migrationFunction = Just "migrateAll" })
	  [groundhog|
  - entity: DbObsData
  - entity: MPCObs.Observatory
  - entity: DbObserver
  - entity: DbRec
|]

{-
mkPersist (defaultCodegenConfig { namingStyle = persistentNamingStyle
				, migrationFunction = Just "migrateAll" })
	  [groundhog|
  - entity: DbObsData
  - entity: MPCObs.Observatory
  - entity: DbObserver
  - entity: DbProvDesign
  - entity: DbRec
|]
-}
