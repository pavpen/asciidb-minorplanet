{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.LowellObservatory.ObjSurveys
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write orbit surveys fields of records used in asteroid orbit files
-- ('astorb.dat'), The Asteroid Orbital Elements Database, produced by the
-- Lowell Observatory <http://lowell.edu/>.  See
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

module Data.ASCII.LowellObservatory.ObjSurveys where

import Data.ASCII.Get		(readWithWidth)
import Data.Bits		((.|.), (.&.), testBit)
import Data.Binary.Get		(Get (..))


data ObjSurveys = ObjSurveys
	{ pl :: Bool   -- Mask  1, Palomar-Leiden survey
	, t1 :: Bool   -- Mask 16, Palomar-Leiden T-1 survey
	, t2 :: Bool   -- Mask  2, Palomar-Leiden T-2 survey
	, t3 :: Bool   -- Mask  4, Palomar-Leiden T-3 survey
	, ucas :: Bool -- Mask  8, U.K. Schmidt Telescope-Caltech survey (UCAS)
	} deriving (Show, Eq)


mkFromInt i = ObjSurveys
	{ pl    = i .&.  1 /= 0
	, t1    = i .&. 16 /= 0
	, t2    = i .&.  2 /= 0
	, t3    = i .&.  4 /= 0
	, ucas  = i .&.  8 /= 0
	}



instance Enum ObjSurveys where
  toEnum = mkFromInt
  fromEnum (ObjSurveys {..}) =
	bit1 .|. bit2 .|. bit3 .|. bit4 .|. bit5
    where bit1 | pl = 1
  	       | otherwise = 0
	  bit2 | t2 = 2
	       | otherwise = 0
	  bit3 | t3 = 4
	       | otherwise = 0
	  bit4 | ucas = 8
	       | otherwise = 0
	  bit5 | t1 = 16
	       | otherwise = 0

instance Num ObjSurveys where
  a + b = toEnum $ (fromEnum a) + (fromEnum b)
  a - b = toEnum $ (fromEnum a) - (fromEnum b)
  a * b = toEnum $ (fromEnum a) * (fromEnum b)
  abs = toEnum . abs . fromEnum
  signum = toEnum . signum . fromEnum
  fromInteger = toEnum . fromIntegral

instance Ord ObjSurveys where
  compare a b = compare (fromEnum a) (fromEnum b)

instance Real ObjSurveys where
  toRational = toRational . fromEnum

instance Integral ObjSurveys where
  toInteger = fromIntegral . fromEnum
  quotRem a b = let (qa, qb) = quotRem (fromEnum a) (fromEnum b)
  		in (toEnum qa, toEnum qb)

getObjSurveys :: Get ObjSurveys
getObjSurveys = do
	spec <- readWithWidth 4
	return $ toEnum spec
