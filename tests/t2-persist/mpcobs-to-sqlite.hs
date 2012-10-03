{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Monad.IO.Class		(liftIO)
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Binary			(decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar		(fromGregorian)
import Database.Groundhog		(PersistEntity (..), runMigration,
					 defaultMigrationLogger, get, insert,
					 selectAll)
import Database.Groundhog.Sqlite	(runSqliteConn, withSqliteConn)
import qualified Database.Persist.MinorPlanetCenter as MPCDb
import System.Environment		(getProgName)


main = do
	progName <- getProgName
	putStrLn $ "Hi; " ++ progName ++ " here."
	inpBs <- LBS.readFile "../t1-read/UnnObs.txt.gz"
	let inpRecs = MPCObs.getRecs $ decompress inpBs
	withSqliteConn "db/mpcobs.sqlite" $ runSqliteConn $ do
	  runMigration defaultMigrationLogger MPCDb.migrateAll
	  mapM_ procsRec inpRecs
  where procsRec rec = do
  		recId <- insert $ MPCDb.toDbRec rec
		liftIO $ putStrLn $ (show recId) ++ ": "
		liftIO $ LCh8.putStrLn $ encode rec
