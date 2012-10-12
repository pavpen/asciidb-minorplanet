{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings, QuasiQuotes,
    RecordWildCards, TemplateHaskell, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.Persist.MinorPlanetCenter
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- 'Persistent' datatypes and mapping to the 'Obs.Rec' datatype, and those of
-- its fields.  (I.e. this is the bridge to, e.g. SQL, database backends via
-- the 'groundhog' package.)
--
-----------------------------------------------------------------------------

module Database.Persist.MinorPlanetCenter
  ( module Database.Persist.MinorPlanetCenter.DbTypes
  , module Database.Persist.MinorPlanetCenter.Selectors
  ) where

import Database.Persist.MinorPlanetCenter.DbTypes
import Database.Persist.MinorPlanetCenter.Selectors
