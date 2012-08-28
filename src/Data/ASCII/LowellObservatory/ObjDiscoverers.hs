{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.LowellObservatory.ObjDiscoverers
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write object discoverers fields of records used in asteroid orbit
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
-- The creation and maintenance of this module has nothing to do with the
-- Lowell Observatory, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.LowellObservatory.ObjDiscoverers where

import Data.ASCII.Get		(readWithWidth)
import Data.Bits		((.|.), (.&.), testBit)
import Data.Binary.Get		(Get (..))


data ObjDiscoverers = ObjDiscoverers
	{ bowell :: Bool	-- Mask 1
	, lowellOther :: Bool	-- Mask 2
	} deriving (Show, Eq)


mkFromInt i = ObjDiscoverers
	{ bowell 	= i .&. 1 /= 0
	, lowellOther	= i .&. 2 /= 0
	}



instance Enum ObjDiscoverers where
  toEnum = mkFromInt
  fromEnum (ObjDiscoverers {..}) =
	bit1 .|. bit2
    where bit1 | bowell = 1
  	       | otherwise = 0
	  bit2 | lowellOther = 2
	       | otherwise = 0

instance Num ObjDiscoverers where
  a + b = toEnum $ (fromEnum a) + (fromEnum b)
  a - b = toEnum $ (fromEnum a) - (fromEnum b)
  a * b = toEnum $ (fromEnum a) * (fromEnum b)
  abs = toEnum . abs . fromEnum
  signum = toEnum . signum . fromEnum
  fromInteger = toEnum . fromIntegral

instance Ord ObjDiscoverers where
  compare a b = compare (fromEnum a) (fromEnum b)

instance Real ObjDiscoverers where
  toRational = toRational . fromEnum

instance Integral ObjDiscoverers where
  toInteger = fromIntegral . fromEnum
  quotRem a b = let (qa, qb) = quotRem (fromEnum a) (fromEnum b)
  		in (toEnum qa, toEnum qb)

getObjDiscoverers :: Get ObjDiscoverers
getObjDiscoverers = do
	spec <- readWithWidth 4
	return $ toEnum spec
