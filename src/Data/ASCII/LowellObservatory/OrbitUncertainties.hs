{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.LowellObservatory.OrbitUncertainties
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write orbit uncertainties ('Orbit computation') fields of records
-- used in asteroid orbit files ('astorb.dat'), The Asteroid Orbital Elements
-- Database, produced by the Lowell Observatory <http://lowell.edu/>.  See
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

module Data.ASCII.LowellObservatory.OrbitUncertainties where

import Data.ASCII.Get		(readWithWidth)
import Data.Binary.Get		(Get (..))
import Data.Bits		((.|.), (.&.), testBit)


data OrbitUncertainties = OrbitUncertainties
	{ uncertainObsLink :: Bool	   -- Mask   1
	, eccAssumed :: Bool		   -- Mask   2
	, eccAndSemiMajorAssumed :: Bool   -- Mask   4
	, omissionOQPDegradation :: Bool   -- Mask   8
	, unsubstantOQPDegradation :: Bool -- Mask  16
	, notInMPC :: Bool		   -- Mask  32
	, magUnknown :: Bool		   -- Mask  64
	, notFound :: Bool		   -- Mask 128
	} deriving (Show, Eq)


mkFromInt i = OrbitUncertainties
	{ uncertainObsLink	   = i .&.   1 /= 0
	, eccAssumed		   = i .&.   2 /= 0
	, eccAndSemiMajorAssumed   = i .&.   4 /= 0
	, omissionOQPDegradation   = i .&.   8 /= 0
	, unsubstantOQPDegradation = i .&.  16 /= 0
	, notInMPC		   = i .&.  32 /= 0
	, magUnknown		   = i .&.  64 /= 0
	, notFound		   = i .&. 128 /= 0
	}



instance Enum OrbitUncertainties where
  toEnum = mkFromInt
  fromEnum (OrbitUncertainties {..}) =
	bit1 .|. bit2 .|. bit3 .|. bit4 .|. bit5 .|. bit6 .|. bit7 .|. bit8
    where bit1 | uncertainObsLink = 1
  	       | otherwise = 0
	  bit2 | eccAssumed = 2
	       | otherwise = 0
	  bit3 | eccAndSemiMajorAssumed = 4
	       | otherwise = 0
	  bit4 | omissionOQPDegradation = 8
	       | otherwise = 0
	  bit5 | unsubstantOQPDegradation = 16
	       | otherwise = 0
	  bit6 | notInMPC = 32
	       | otherwise = 0
	  bit7 | magUnknown = 64
	       | otherwise = 0
	  bit8 | notFound = 128
	       | otherwise = 0

instance Num OrbitUncertainties where
  a + b = toEnum $ (fromEnum a) + (fromEnum b)
  a - b = toEnum $ (fromEnum a) - (fromEnum b)
  a * b = toEnum $ (fromEnum a) * (fromEnum b)
  abs = toEnum . abs . fromEnum
  signum = toEnum . signum . fromEnum
  fromInteger = toEnum . fromIntegral

instance Ord OrbitUncertainties where
  compare a b = compare (fromEnum a) (fromEnum b)

instance Real OrbitUncertainties where
  toRational = toRational . fromEnum

instance Integral OrbitUncertainties where
  toInteger = fromIntegral . fromEnum
  quotRem a b = let (qa, qb) = quotRem (fromEnum a) (fromEnum b)
  		in (toEnum qa, toEnum qb)

getOrbitUncertainties :: Get OrbitUncertainties
getOrbitUncertainties = do
	spec <- readWithWidth 4
	return $ toEnum spec
