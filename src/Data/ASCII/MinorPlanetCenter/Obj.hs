{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.Obj
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Definitions common to minor planets and comets.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.Obj where

import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD


data Design = DesignNum Int
	    | DesignProv PD.ProvDesign
	    deriving (Eq, Show)

class Designatable obj where
  getDesign :: obj -> Design
