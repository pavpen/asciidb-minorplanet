{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.Observatory
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write the observatory fields for minor planet observation records
-- as found in the Minor Planet Center's Observation Archive database.  (See
-- <http://www.minorplanetcenter.net/iau/ECS/MPCAT-OBS/MPCAT-OBS.html>.)
--
-- Some descriptions of the format are:
--
-- 	* <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>
--
-- 	* <http://www.minorplanetcenter.net/iau/info/PackedDes.html>
--
-- 	* <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>
--
-- 	* <http://www.minorplanetcenter.net/iau/info/RovingObs.html>
--
-- The creation and maintenance of this module has nothing to do with the Minor
-- Planet Center, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.Observatory where

data Observatory = ObservatoryCode { obsCode :: String }
		 | RovingObserver { lonRad :: Double
				  , latRad :: Double
				  , altMeters :: Double }
		 deriving (Show, Eq)

observatoryGetCode :: Observatory -> String
observatoryGetCode (ObservatoryCode {..}) | null obsCode = "   "
					  | otherwise    = obsCode
