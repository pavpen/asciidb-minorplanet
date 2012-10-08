{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings, QuasiQuotes,
    RecordWildCards, TemplateHaskell, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.Persist.MinorPlanetCenter.Selectors
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Selectors for the 'Persistent' datatypes.
--
-----------------------------------------------------------------------------

module Database.Persist.MinorPlanetCenter.Selectors where

import Data.ASCII.MinorPlanetCenter.Note2
					(readNote2W8)
import Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Time			(UTCTime)
import Data.Word			(Word8)
import Database.Groundhog		((==.), (&&.), select, project)
import Database.Groundhog.TH		(CodegenConfig (..),
					 defaultCodegenConfig,
					 persistentNamingStyle, mkPersist,
					 groundhog)
import Database.Groundhog.Sqlite	()

import Database.Persist.MinorPlanetCenter.DbTypes



{-
selectDbProvDesign (DbMinorPlanetId {..}) =
	select $ DbMinorPlanetIdYear ==. year &&.
		DbMinorPlanetIdMonth ==. month &&.
		DbMinorPlanetIdMonthSecHalf ==. monthSecHalf &&.
		DbMinorPlanetIdOrder ==. order
selectDbProvDesign (DbSurveyPLId {..}) = select $ DbSurveyPLIdOrder ==. order
selectDbProvDesign (DbSurveyT1Id {..}) = select $ DbSurveyT1IdOrder ==. order
selectDbProvDesign (DbSurveyT2Id {..}) = select $ DbSurveyT2IdOrder ==. order
selectDbProvDesign (DbSurveyT3Id {..}) = select $ DbSurveyT3IdOrder ==. order
selectDbProvDesign (DbCometId {..}) =
	select $ DbCometIdYear ==. year &&.
		DbCometIdMonth ==. month &&.
		DbCometIdMonthSecHalf ==. monthSecHalf &&.
		DbCometIdOrder ==. order &&.
		DbCometIdDbStatus ==. dbStatus &&.
		DbCometIdDbFragment ==. dbFragment

selectProvDesign pd = selectDbProvDesign $ toDbProvDesign pd
-}


{-
projectDbProvDesign p (DbMinorPlanetId {..}) =
	project p $ DbMinorPlanetIdYear ==. year &&.
		DbMinorPlanetIdMonth ==. month &&.
		DbMinorPlanetIdMonthSecHalf ==. monthSecHalf &&.
		DbMinorPlanetIdOrder ==. order
projectDbProvDesign p (DbSurveyPLId {..}) =
	project p $ DbSurveyPLIdOrder ==. order
projectDbProvDesign p (DbSurveyT1Id {..}) =
	project p $ DbSurveyT1IdOrder ==. order
projectDbProvDesign p (DbSurveyT2Id {..}) =
	project p $ DbSurveyT2IdOrder ==. order
projectDbProvDesign p (DbSurveyT3Id {..}) =
	project p $ DbSurveyT3IdOrder ==. order
projectDbProvDesign p (DbCometId {..}) =
	project $ DbCometIdYear ==. year &&.
		DbCometIdMonth ==. month &&.
		DbCometIdMonthSecHalf ==. monthSecHalf &&.
		DbCometIdOrder ==. order &&.
		DbCometIdDbStatus ==. dbStatus &&.
		DbCometIdDbFragment ==. dbFragment
-}
