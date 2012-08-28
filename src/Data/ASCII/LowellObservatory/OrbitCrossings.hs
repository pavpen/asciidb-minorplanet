{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.LowellObservatory.OrbitCrossings
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write orbit crossings fields of records used in asteroid orbit
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

module Data.ASCII.LowellObservatory.OrbitCrossings where

import Data.ASCII.Get		(readWithWidth)
import Data.Binary.Get		(Get (..))
import Data.Bits		((.|.), (.&.), testBit)


data OrbitCrossings = OrbitCrossings
	{ aten :: Bool	 -- Mask  1
	, apollo :: Bool -- Mask  2
	, amor :: Bool	 -- Mask  4
	, mars :: Bool	 -- Mask  8
	, outer :: Bool	 -- Mask 16
	} deriving (Show, Eq)

mkFromInt i = OrbitCrossings
	{ aten   = i .&.  1 /= 0
	, apollo = i .&.  2 /= 0
	, amor   = i .&.  4 /= 0
	, mars   = i .&.  8 /= 0
	, outer  = i .&. 16 /= 0
	}



instance Enum OrbitCrossings where
  toEnum = mkFromInt
  fromEnum (OrbitCrossings {..}) =
	bit1 .|. bit2 .|. bit3 .|. bit4 .|. bit5
    where bit1 | aten = 1
  	       | otherwise = 0
	  bit2 | apollo = 2
	       | otherwise = 0
	  bit3 | amor = 4
	       | otherwise = 0
	  bit4 | mars = 8
	       | otherwise = 0
	  bit5 | outer = 16
	       | otherwise = 0

instance Num OrbitCrossings where
  a + b = toEnum $ (fromEnum a) + (fromEnum b)
  a - b = toEnum $ (fromEnum a) - (fromEnum b)
  a * b = toEnum $ (fromEnum a) * (fromEnum b)
  abs = toEnum . abs . fromEnum
  signum = toEnum . signum . fromEnum
  fromInteger = toEnum . fromIntegral

instance Ord OrbitCrossings where
  compare a b = compare (fromEnum a) (fromEnum b)

instance Real OrbitCrossings where
  toRational = toRational . fromEnum

instance Integral OrbitCrossings where
  toInteger = fromIntegral . fromEnum
  quotRem a b = let (qa, qb) = quotRem (fromEnum a) (fromEnum b)
  		in (toEnum qa, toEnum qb)

getOrbitCrossings :: Get OrbitCrossings
getOrbitCrossings = do
	spec <- readWithWidth 4
	return $ toEnum spec
