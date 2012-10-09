{-# LANGUAGE DeriveDataTypeable, GADTs, FlexibleContexts,
    GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings, QuasiQuotes,
    RecordWildCards, TemplateHaskell, TypeFamilies #-}

import Codec.Compression.GZip		(decompress)
import Control.Applicative		((<$>))
import Control.Monad			(foldM)
import Control.Monad.IO.Class		(liftIO)
import Control.Monad.Reader		(ask)
import Control.Monad.State		(get, put)
import Data.Acid			(Update (..), Query (..),
					 openLocalStateFrom, makeAcidic,
					 update, query)
import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.Binary			(decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.IntMap			(IntMap)
import qualified Data.IntMap as IntMap
import Data.List			(isSuffixOf)
import qualified Data.Map as Map
import Data.SafeCopy			(deriveSafeCopy, base)
import Data.Time.Clock			(UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar		(fromGregorian)
import Data.Typeable			(Typeable)
import System.Environment		(getProgName, getArgs)


-- ACID Database:
data ObsDb = ObsDb
	   { numObs :: IntMap MPCObs.Rec
	   , unnObs :: Map.Map String MPCObs.Rec
	   } deriving Typeable

$(deriveSafeCopy 0 'base ''MPCObs.Observatory)
$(deriveSafeCopy 0 'base ''MPCObs.SatObsUnitsToUse)
$(deriveSafeCopy 0 'base ''MPCObs.SigReturnPt)
$(deriveSafeCopy 0 'base ''MPCObs.ObsData)
$(deriveSafeCopy 0 'base ''MPCObs.Observer)
$(deriveSafeCopy 0 'base ''MPCObs.ProvDesign)
$(deriveSafeCopy 0 'base ''MPCObs.Rec)
$(deriveSafeCopy 0 'base ''ObsDb)

-- Database transactions:
obsDbAddObs :: MPCObs.Rec -> Update ObsDb ()
obsDbAddObs rec@(MPCObs.Rec {objNumber, provDesign, time}) = do
	db <- get
	put $ upd db
  where upd db@(ObsDb {..})
  	  | objNumber == -1 = do
	    let (Just pd) = provDesign
  	    db { unnObs = Map.insertWith chooseRec (PD.showPacked pd) rec unnObs
	       }
	  | otherwise =
  	    db { numObs = IntMap.insertWith chooseRec objNumber rec numObs }
	chooseRec oldR@(MPCObs.Rec {time=oldT}) newR@(MPCObs.Rec {time=newT})
	  | newT < oldT = newR
	  | otherwise   = oldR

obsDbLookupObs :: Int -> Maybe PD.ProvDesign -> Query ObsDb (Maybe MPCObs.Rec)
obsDbLookupObs objNumber provDesign = do
	db <- ask
	return $ lkup db
  where lkup db@(ObsDb {..})
  	  | objNumber == -1 = do
	    let (Just pd) = provDesign
	    Map.lookup (PD.showPacked pd) unnObs
	  | otherwise = IntMap.lookup objNumber numObs

obsDbGetDb :: Query ObsDb ObsDb
obsDbGetDb = ask

obsDbPutDb :: ObsDb -> Update ObsDb ()
obsDbPutDb = put

$(makeAcidic ''ObsDb ['obsDbAddObs, 'obsDbLookupObs, 'obsDbGetDb, 'obsDbPutDb])


zeroRec = MPCObs.Rec
	{ objNumber = -1
	, provDesign = Nothing
	, discovery = False
	, note1 = ' '
	, j2000Adj = True
	, time = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
	, observer = MPCObs.OpticalObs
		{ obsData = MPCObs.ObsData
			  { rightAscSec = 0.0 / 0.0
			  , declRad = 0.0 / 0.0
			  , magnitude = Nothing
			  , obsBand = ' '
			  , rfcCode = "     "
			  } }
	, observatory = MPCObs.ObservatoryCode { obsCode = "000" }
	}


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
	obsDb <- openLocalStateFrom
		  "db/mpcobs-initial-acid"
		  (ObsDb { numObs = IntMap.empty, unnObs = Map.empty })
	mapM_ (procsRec obsDb) filtRecs
	-- db <- query obsDb (ObsDbGetDb)
	-- newDb <- foldM procsRec' db filtRecs
	-- update obsDb (ObsDbPutDb db)
  where procsRec obsDb rec@(MPCObs.Rec {objNumber, provDesign}) = do
  		-- oldRec <- query obsDb (ObsDbLookupObs objNumber provDesign)
		-- case oldRec of
		--  Nothing -> LCh8.putStrLn $ encode rec
		--  _ ->  return ()
  		update obsDb (ObsDbAddObs rec)
		newRec <- query obsDb (ObsDbLookupObs objNumber provDesign)
		putStrLn $ LCh8.unpack $ encode newRec
		return ()
	procsRec' db rec@(MPCObs.Rec {objNumber, provDesign}) = do
		let oldRec = lookupRec db objNumber provDesign
		resDb@(ObsDb {..}) <- updRec db rec
		case oldRec of
		  Nothing -> LCh8.putStrLn $ encode rec
		  _ ->  return ()
		return resDb
	updRec db@(ObsDb {..}) rec@(MPCObs.Rec {objNumber, provDesign})
  	  | objNumber == -1 = do
	    let (Just pd) = provDesign
  	    return $ db { unnObs =
	    		Map.insertWith chooseRec (PD.showPacked pd) rec unnObs }
	  | otherwise = return $
  	    db { numObs = IntMap.insertWith chooseRec objNumber rec numObs }
	chooseRec oldR@(MPCObs.Rec {time=oldT}) newR@(MPCObs.Rec {time=newT})
	  | newT < oldT = newR
	  | otherwise   = oldR
	lookupRec (ObsDb {..}) objNumber provDesign
  	  | objNumber == -1 = do let (Just pd) = provDesign
				 Map.lookup (PD.showPacked pd) unnObs
	  | otherwise = IntMap.lookup objNumber numObs
