-----------------------------------------------------------------------------
-- |
-- Module      : Data.Conversion
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- The creation and maintenance of this module was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.Conversion where

class SIConvertible t where
  toSIFactor :: (Fractional n) => t -> n
  toSIFactor t = 1 / (fromSIFactor t)
  fromSIFactor :: (Fractional n) => t -> n
  fromSIFactor t = 1 / (toSIFactor t)
