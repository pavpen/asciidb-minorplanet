{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving,
    NamedFieldPuns, OverloadedStrings, QuasiQuotes, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Monad.IO.Class		(liftIO)
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Binary			(decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.List			(isSuffixOf)
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar		(fromGregorian)
import Database.Groundhog		(PersistEntity (..), Order (..), (=.),
					 (==.), runMigration,
					 defaultMigrationLogger, get, insert,
					 select, selectAll, update, delete,
					 orderBy)
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
	    filtRecs = inpRecs -- filter MPCObs.discovery inpRecs
	withSqliteConn "db/mpcobs-initial.sqlite" $ runSqliteConn $ do
	  runMigration defaultMigrationLogger MPCDb.migrateAll
	  mapM_ procsRec filtRecs
  where procsRec rec@(MPCObs.Rec {objNumber, provDesign, time}) = do
  		prevRec <- getDesignRec objNumber provDesign
		case (map MPCDb.fromDbRec prevRec) of
		  (MPCObs.Rec {time = t}:_) ->
		    if time < t then updDesignRec objNumber provDesign rec
				else return ()
		  _ -> insRec rec
	getDesignRec (-1) (Just provDesign) =
		select $ (MPCDb.DbRecDbProvDesign ==.
			  (Just $ MPCDb.toDbProvDesign provDesign)) `orderBy`
			 [Asc MPCDb.DbRecTime]
	getDesignRec objNumber _ =
		select $ (MPCDb.DbRecObjNumber ==. objNumber) `orderBy`
			 [Asc MPCDb.DbRecTime]
	{-
	updDesignRec (-1) (Just pd) (MPCObs.Rec {..}) =
		update [MPCDb.DbRecDiscovery =. discovery,
			MPCDb.DbRecDbNote1 =. fromEnum note1,
			MPCDb.DbRecJ2000Adj =. j2000Adj,
			MPCDb.DbRecTime =. time,
			MPCDb.DbRecDbObserver =. MPCDb.toDbObserver observer,
			MPCDb.DbRecObservatory =. observatory]
		       $ MPCDb.DbRecDbProvDesign ==.
				(Just $ MPCDb.toDbProvDesign pd)
	updDesignRec objNum _ (MPCObs.Rec {..}) =
		update [MPCDb.DbRecDiscovery =. discovery,
			MPCDb.DbRecDbNote1 =. fromEnum note1,
			MPCDb.DbRecJ2000Adj =. j2000Adj,
			MPCDb.DbRecTime =. time,
			MPCDb.DbRecDbObserver =. MPCDb.toDbObserver observer,
			MPCDb.DbRecObservatory =. observatory]
		       $ MPCDb.DbRecObjNumber ==. objNum
	-}
	updDesignRec objNumber provDesign rec = do
		delDesignRec objNumber provDesign
		insRec rec
	delDesignRec (-1) (Just provDesign) =
		delete $ MPCDb.DbRecDbProvDesign ==.
			  (Just $ MPCDb.toDbProvDesign provDesign)
	delDesignRec objNumber _ =
		delete $ MPCDb.DbRecObjNumber ==. objNumber
	insRec rec = do
  		recId <- insert $ MPCDb.toDbRec rec
		liftIO $ putStrLn $ (show recId) ++ ": "
		liftIO $ LCh8.putStrLn $ encode rec
