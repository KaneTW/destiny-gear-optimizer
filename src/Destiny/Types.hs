 
{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Destiny.Types where

import GHC.Generics

data GearSet a = GearSet
  { kinetic :: a
  , energy :: a
  , power :: a
  , head :: a
  , glove :: a
  , chest :: a
  , leg :: a
  , classItem :: a
  } deriving stock (Show, Eq, Ord, Generic, Functor, Foldable)


-- don't really care for the particulars, just how many (assuming uniform)
type DropTable = GearSet Int

type Level = Int
type SlotTable = GearSet Level