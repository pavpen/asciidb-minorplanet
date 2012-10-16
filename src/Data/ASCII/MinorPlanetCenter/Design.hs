{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.Design
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Minor planet and comet designations.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.Design where

import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD
import Data.ASCII.MinorPlanetCenter.Util
				(toAlNumStrWithWidth)


data Design = DesignNum Int
	    | DesignProv PD.ProvDesign
	    deriving (Eq, Show)

class Designatable obj where
  getDesign :: obj -> Design

showLong :: Design -> String
showLong (DesignNum n) = (show n)
showLong (DesignProv pd) = PD.showLong pd

showPacked :: Design -> String
showPacked (DesignNum n) = toAlNumStrWithWidth 5 n
showPacked (DesignProv pd) = PD.showPacked pd
