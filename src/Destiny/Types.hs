 
{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable, ConstraintKinds, Strict #-}
module Destiny.Types where

import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Word
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Generics
import Data.Vector.Instances
import Data.Store
import Control.DeepSeq



import qualified Control.Concurrent.STM.Map as SM

data Slot 
  = Kinetic | Energy | Power
  | Head | Glove | Chest | Leg | ClassItem
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  

type GearSet a = Map Slot a

-- don't really care for the particulars, just how many (assuming uniform)
-- nb: per slot or per item?
newtype DropChances = DropChances Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Integral)

type DropTable = GearSet DropChances

newtype PowerLevel = PowerLevel Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Integral, Read)

type PowerLevelDelta = PowerLevel

-- | This must be a total map.
type SlotTable = GearSet PowerLevel

data Activity = Activity 
  { powerGain :: !Int
  , names :: ![String] -- ^ arity = len names
  , pdf :: !(M.Map Slot Double)
  } deriving stock (Show, Eq, Ord)

type Actions = V.Vector Activity

data StateEntry = StateEntry
  { meanSlotDeviation :: !(VU.Vector Word8) -- ^ bounded to sum < 8
  , availableActions :: !(VU.Vector Word8) -- ^ index corresponds to index in Actions vector, value is arity (how often can you take that action)
  } deriving stock (Show, Eq, Ord, Generic)

instance Hashable StateEntry
instance Store StateEntry
instance NFData StateEntry

type MDPState = HM.HashMap StateEntry (Int, Double)