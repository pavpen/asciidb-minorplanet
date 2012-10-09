{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    NamedFieldPuns, OverloadedStrings, QuasiQuotes, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

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
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime,
					 diffUTCTime)
import Data.Time.Clock.POSIX		(utcTimeToPOSIXSeconds)
import Data.Time.Calendar		(fromGregorian)
import Database.Groundhog		(PersistEntity (..), Order (..), (==.),
					 (&&.), (||.), runMigration,
					 defaultMigrationLogger, get, insert,
					 select, selectAll, orderBy)
import Database.Groundhog.Sqlite	(runSqliteConn, withSqliteConn)
import qualified Database.Persist.MinorPlanetCenter as MPCDb
import System.Environment		(getProgName, getArgs)


domineT = UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)
epochT = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime $ 12 * 3600)

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
	withSqliteConn "../t2-persist/db/mpcobs-filtered.sqlite" $
	 runSqliteConn $ do
	  runMigration defaultMigrationLogger MPCDb.migrateAll
	  mapM_ procsRec inpRecs
  where procsRec rec@(AstOrb.Rec {..}) = do
	  -- liftIO $ LCh8.putStrLn $ encode rec
  	  obs@(MPCDb.DbRec { time }) <- getDiscovObs rec
	  -- liftIO $ print obs
	  liftIO $ putStrLn $ "{ " ++
	  	(show semiMajAAU) ++ ", " ++
	  	(show ecc) ++ ", " ++
		(show $ inclRad * 180 / pi) ++ ", " ++
		(show $ lngAscNodeRad * 180 / pi) ++ ", " ++
		(show $ argPerihRad * 180 / pi) ++ ", " ++
		(show $ meanAnomRad * 180 / pi) ++ ", " ++
		(show $ round $ time `diffUTCTime` epochT) ++ " },"
	getDiscovObs (AstOrb.Rec {..})
	  | objNumber == -1 =
	    do let pd = MPCDb.toDbProvDesign $ PD.readLong designation
	       -- liftIO $ putStrLn $ "getDiscovObs: objNumber is -1, pd: " ++
	       --				(show pd)
	       recs <- select $ (MPCDb.DbRecDbProvDesign ==. Just pd) `orderBy`
				[Desc MPCDb.DbRecTime]
	       -- liftIO $ putStrLn $ "Discovery recs: "
	       -- liftIO $ mapM_ (print . encode . MPCDb.fromDbRec) recs
	       return $ head recs
	  | otherwise =
	    do -- liftIO $ putStrLn $ "getDiscovObs: objNumber is " ++
	       --				(show objNumber)
	       recs <- select $ (MPCDb.DbRecObjNumber ==. objNumber) `orderBy`
				[Desc MPCDb.DbRecTime]
	       -- liftIO $ putStrLn $ "Discovery recs: "
	       -- liftIO $ mapM_ (print . encode . MPCDb.fromDbRec) recs
	       return $ head recs
