{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleContexts,
    GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings, QuasiQuotes,
    RecordWildCards, TemplateHaskell, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.Persist.MinorPlanetCenter
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- 'Persistent' datatypes and mapping to the 'Obs.Rec' datatype, and those of
-- its fields.  (I.e. this is the bridge to, e.g. SQL, database backends via
-- the 'persistent' package from Yesod.)
--
-----------------------------------------------------------------------------

module Database.Persist.MinorPlanetCenter where

import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Time			(UTCTime)
import Data.Word			(Word8)
import Database.Persist			(PersistEntityBackend (..),
					 PersistEntity (..), PersistField (..))
import Database.Persist.TH		(persist, mkPersist, mkMigrate, share,
					 sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
SQLRec
	objNumber Int	  -- Use -1 for unnumbered objects.
	provDesign String -- Use packed provisional designations, possibly ""
	discovery Bool
	note1 Word8
	note2 Word8
	time UTCTime
	rightAscSec Double
	declRad Double
	magnitude Double Maybe
	obsBand Word8
	rfcCode String
	observatoryCode String
	deriving Show
|]


fromDbEntity (SQLRec objNumber provDesign discovery note1 note2 time
		rightAscSec declRad magnitude obsBand rfcCode observatoryCode) =
	MPCObs.Rec
	{ MPCObs.objNumber = objNumber
	, MPCObs.provDesign = case provDesign of
				"" -> Nothing
				_  -> Just $ PD.readPacked provDesign
	, MPCObs.discovery = discovery
	, MPCObs.note1 = toEnum $ fromIntegral note1
	, MPCObs.note2 = toEnum $ fromIntegral note2
	, MPCObs.time = time
	, MPCObs.rightAscSec = rightAscSec
	, MPCObs.declRad = declRad
	, MPCObs.magnitude = magnitude
	, MPCObs.obsBand = toEnum $ fromIntegral obsBand
	, MPCObs.rfcCode = rfcCode
	, MPCObs.observatoryCode = observatoryCode
	}


toDbEntity (MPCObs.Rec {..}) =
	SQLRec objNumber
		(maybe "" (\d -> PD.showPacked d) provDesign)
		discovery
		(fromIntegral $ fromEnum note1)
		(fromIntegral $ fromEnum note2)
		time
		rightAscSec
		declRad
		magnitude
		(fromIntegral $ fromEnum obsBand)
		rfcCode
		observatoryCode
