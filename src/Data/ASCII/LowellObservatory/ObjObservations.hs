{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.LowellObservatory.ObjObservations
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write orbit observations fields of records used in asteroid orbit
-- files ('astorb.dat'), The Asteroid Orbital Elements Database, produced by
-- the Lowell Observatory <http://lowell.edu/>.  See
-- <ftp://ftp.lowell.edu/pub/elgb/astorb.html>.
--
-- *Acknowledgment and Attribution*
-- The research and computing needed to generate <astorb.dat> were funded
-- principally by NASA grant NAG5-4741, and in part by the Lowell Observatory
-- endowment. <astorb.dat> may be freely used, copied, and transmitted provided
-- attribution to Dr. Edward Bowell and the aforementioned funding sources is
-- made.
--
-- The creation and maintenance of this I/O module has nothing to do with the
-- Lowell Observatory, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.LowellObservatory.ObjObservations where

import Data.ASCII.Get		(readWithWidth)
import Data.Bits		((.|.), (.&.), testBit)
import Data.Binary.Get		(Get (..))


data ObjObservations = ObsNone		  -- 0
		     | ObsObjLost	  -- 1
		     | Obs2Times	  -- 2
		     | Obs3Times	  -- 3
		     | Obs4P10yAgo	  -- 4
		     | Obs4POnceInLast10y -- 5
		     | Obs4POther	  -- 6
		     | ObsPoorMagMeasure  -- 7
	deriving (Show, Eq)

instance Enum ObjObservations where
  fromEnum ObsNone		= 0
  fromEnum ObsObjLost		= 1
  fromEnum Obs2Times		= 2
  fromEnum Obs3Times		= 3
  fromEnum Obs4P10yAgo		= 4
  fromEnum Obs4POnceInLast10y	= 5
  fromEnum Obs4POther		= 6
  fromEnum ObsPoorMagMeasure	= 7

  toEnum 0 = ObsNone
  toEnum 1 = ObsObjLost
  toEnum 2 = Obs2Times
  toEnum 3 = Obs3Times
  toEnum 4 = Obs4P10yAgo
  toEnum 5 = Obs4POnceInLast10y
  toEnum 6 = Obs4POther
  toEnum 7 = ObsPoorMagMeasure

getObjObservations :: Get ObjObservations
getObjObservations = do
	spec <- readWithWidth 4
	return $ toEnum spec
