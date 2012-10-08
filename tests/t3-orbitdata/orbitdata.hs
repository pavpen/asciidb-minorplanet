{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    OverloadedStrings, QuasiQuotes, RecordWildCards, TemplateHaskell,
    TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Monad			(forM_)
import Control.Monad.IO.Class		(liftIO)
import qualified Data.ASCII.LowellObservatory.AstOrb as AstOrb
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Binary			(encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.List			(isSuffixOf)
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar		(fromGregorian)
import Database.Groundhog		(PersistEntity (..), (==.), (&&.),
					 (||.), runMigration,
					 defaultMigrationLogger, get, insert,
					 select, selectAll)
import Database.Groundhog.Sqlite	(runSqliteConn, withSqliteConn)
import qualified Database.Persist.MinorPlanetCenter as MPCDb
import System.Environment		(getProgName, getArgs)


main = do
	progName <- getProgName
	putStrLn $ "Hi!  " ++ progName ++ " here!"
	args <- getArgs
	let inpFPath = case args of
			(path:_) -> path
			_        -> "../t1-read/astorb.dat.gz"
	let decompressor | ".gz" `isSuffixOf` inpFPath = decompress
			 | otherwise = id
	bs <- LBS.readFile inpFPath
	let inpRecs = AstOrb.getRecs $ decompressor bs
	withSqliteConn "../t2-persist/db/mpcobs-discov.sqlite" $ runSqliteConn $
	 do
	  runMigration defaultMigrationLogger MPCDb.migrateAll
	  mapM_ procsRec inpRecs
  where procsRec rec = do
  	  obs <- getDiscovObs rec
	  liftIO $ print obs
	  liftIO $ LCh8.putStrLn $ encode rec
	getDiscovObs (AstOrb.Rec {..})
	  | objNumber == -1 =
	    do let pd = MPCDb.toDbProvDesign $ PD.readLong designation
	       recs <- select $ MPCDb.DbRecDbProvDesign ==. Just pd &&.
	       			MPCDb.DbRecDiscovery ==. True
	       liftIO $ putStrLn $ "Discovery recs: "
	       liftIO $ mapM_ (print . encode . MPCDb.fromDbRec) recs
	       return $ head recs
	  | otherwise =
	    do recs <- select $ MPCDb.DbRecObjNumber ==. objNumber &&.
	    			MPCDb.DbRecDiscovery ==. True
	       return $ head recs
