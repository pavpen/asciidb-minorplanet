{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Control.Monad.IO.Class		(liftIO)
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar		(fromGregorian)
import Database.Persist			(get, insert)
import Database.Persist.GenericSql	(runSqlConn, runMigration)
import Database.Persist.Sqlite		(withSqliteConn)
import qualified Database.Persist.MinorPlanetCenter as MPCDb


tRec = MPCObs.Rec
	{ MPCObs.objNumber = -1
	, MPCObs.provDesign = Just $ PD.SurveyT3Id 4
	, MPCObs.discovery = False
	, MPCObs.note1 = ' '
	, MPCObs.note2 = ' '
	, MPCObs.time = UTCTime (fromGregorian 2012 08 30)
				(secondsToDiffTime 3600)
	, MPCObs.rightAscSec = 1
	, MPCObs.declRad = 2
	, MPCObs.magnitude = Just 3
	, MPCObs.obsBand = ' '
	, MPCObs.rfcCode = "~000"
	, MPCObs.observatoryCode = "001"
	}

main = do
	putStrLn "Hi"
	withSqliteConn "db/t1.sqlite" $ runSqlConn $ do
	  runMigration MPCDb.migrateAll
	  recId <- insert $ MPCDb.toDbEntity tRec
	  dbRec <- get recId
	  liftIO $ print dbRec
