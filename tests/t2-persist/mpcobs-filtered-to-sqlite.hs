{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    NamedFieldPuns, OverloadedStrings, QuasiQuotes, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Monad			(foldM_)
import Control.Monad.IO.Class		(liftIO)
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Binary			(decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.List			(isSuffixOf)
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar		(fromGregorian)
import Database.Groundhog		(PersistEntity (..), runMigration,
					 defaultMigrationLogger, get, insert,
					 selectAll)
import Database.Groundhog.Sqlite	(runSqliteConn, withSqliteConn)
import qualified Database.Persist.MinorPlanetCenter as MPCDb
import System.Environment		(getProgName, getArgs)


main = do
	progName <- getProgName
	putStrLn $ "Hi; " ++ progName ++ " here."
	args <- getArgs
	let inpFPath = case args of
			(path:_) -> path
			_	 -> "../t1-read/UnnObs.txt.gz"
	inpBs <- LBS.readFile inpFPath
	let decompressor | ".gz" `isSuffixOf` inpFPath = decompress
			 | otherwise = id
	let inpRecs = MPCObs.getRecs $ decompressor inpBs
	withSqliteConn "db/mpcobs-filtered.sqlite" $ runSqliteConn $ do
	  runMigration defaultMigrationLogger MPCDb.migrateAll
	  -- mapM_ procsRec inpRecs
	  foldM_ procsRec' ((-1),Nothing) inpRecs
  where procsRec rec = do
  		recId <- insert $ MPCDb.toDbRec rec
		liftIO $ putStrLn $ (show recId) ++ ": "
		liftIO $ LCh8.putStrLn $ encode rec
	procsRec' rId@(objN, provD) rec@(MPCObs.Rec {objNumber, provDesign})
	  | objN == objNumber && provD == provDesign = return rId
	  | otherwise = do
  		recId <- insert $ MPCDb.toDbRec rec
		liftIO $ putStrLn $ (show recId) ++ ": "
		liftIO $ LCh8.putStrLn $ encode rec
		return $ (objNumber, provDesign)
