{-# LANGUAGE Strict #-}
module Destiny.Drops where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified Control.Monad.Parallel as MP
import Control.Monad.Reader
import Data.Function
import Data.Generics.Product
import Data.Word
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Foldable as F
import Debug.Trace

import Control.Monad.ST
import qualified Data.HashTable as HT

import Destiny.Types

-- | Use this to construct total SlotTables; cba to do type shenanigans right now
mkSlotTable :: PowerLevel -> PowerLevel -> PowerLevel -> PowerLevel 
            -> PowerLevel -> PowerLevel -> PowerLevel -> PowerLevel
            -> SlotTable
mkSlotTable kinetic energy power head glove chest leg classItem 
  = M.fromList [ (Kinetic, kinetic), (Energy, energy), (Power, power)
               , (Head, head), (Glove, glove), (Chest, chest), (Leg, leg), (ClassItem, classItem) ]


currentLevel :: SlotTable -> PowerLevel
currentLevel slots = PowerLevel $ floor (x/n)
  where
    x = fromIntegral $ sum slots
    n = fromIntegral $ length slots

flatten :: SlotTable -> SlotTable
flatten slots = fmap (max current) slots
  where
    current = currentLevel slots

fullFlatten :: SlotTable -> SlotTable
fullFlatten = until (\st -> st == flatten st) flatten

-- | Get a reward for +n power, given some slot distribution and slot
reward :: PowerLevel -> Slot -> SlotTable -> SlotTable
reward delta target slots = M.insert target result slots
  where
    result = max (currentLevel slots + delta) (slots M.! target)

-- | Distance between two slot tables. 
-- | nb: absolute value; since this is monotonically increasing it shouldn't matter (when used to calculate reward)
distance :: SlotTable -> SlotTable -> PowerLevel
distance s1 s2 = sum $ map (\slot -> abs $ s2 M.! slot - s1 M.! slot) [minBound..maxBound]



-- consider the state space s = (S', A) where S' is the reduced slot data and A is the set of taken actions
-- Given a concrete slot data S = (x_1, ..., x_8), the mean slot deviation is bounded.
-- After flattening, consider mx = floor (mean S). Then sum S-mx < 8 (proof: imagine sum S-mx = 8. then mx would have had to be (mx+1), but then S-mx = 0. contradiction)
-- This reduces the size of the state space S' to the number of weak 8-compositions of [0..7], 6435

-- actions are encoded by arity (how often can you do an action). in this case, we're looking at about 5*4*3*2*2*3*2 = 1440 states. total of just about 9mn states. real state table has 8mn states, so some are impossible (we're pretty close though)


defaultActions :: Actions
defaultActions = V.fromList
  [ Activity { powerGain = 1, names = ["Hawthorne Weekly", "Vanguard", "Crucible", "Gambit"], pdf = defaultPdf }
  , Activity { powerGain = 2, names = ["Master NH", "100k", "Seasonal"], pdf = defaultPdf }
  , Activity { powerGain = 2, names = ["Pit Final", "Prophecy Final"], pdf = armorPdf }
  , Activity { powerGain = 2, names = ["Raid 1", "Raid 1 Challenge"], pdf = raid1Pdf }
  , Activity { powerGain = 2, names = ["Raid 2", "Raid 2 Challenge"], pdf = raid2Pdf }
  , Activity { powerGain = 2, names = ["Raid 3", "Raid 3 Challenge"], pdf = raid3Pdf }
  , Activity { powerGain = 2, names = ["Raid 4", "Raid 4 Challenge"], pdf = raid4Pdf }
  ]
  where
    defaultPdf = M.fromList
      [ (Kinetic, 1/8), (Energy, 1/8), (Power, 1/8), (Head, 1/8), (Glove, 1/8)
      , (Chest, 1/8), (Leg, 1/8), (ClassItem, 1/8)
      ]
    armorPdf = M.fromList
      [ (Head, 1/5), (Glove, 1/5), (Chest, 1/5), (Leg, 1/5), (ClassItem, 1/5) ]

    raid1Pdf = M.fromList [ (Kinetic, 1/3), (Energy, 1/3), (Leg, 1/3) ]
    raid2Pdf = M.fromList [ (Energy, 1/2), (Glove, 1/2) ]
    raid3Pdf = M.fromList [ (Kinetic, 1/3), (Energy, 1/3), (Chest, 1/3) ]
    raid4Pdf = M.fromList [ (Energy, 1/3), (Head, 1/3), (ClassItem, 1/3) ]

actionSize :: Actions -> Int
actionSize = foldr (\act n -> n * (length (names act) + 1)) 1

mkInitialActions :: Actions -> VP.Vector Word8
mkInitialActions = VP.convert . fmap (fromIntegral.length.names) 

mkState :: SlotTable -> Actions -> StateEntry
mkState slots actions = StateEntry 
  { meanSlotDeviation = VP.fromList $ map (\n -> fromIntegral (n - currentLevel slots)) $ M.elems slots
  , availableActions = mkInitialActions actions
  }

selectAction :: StateEntry -> ReaderT MDPState IO (Maybe StateTransition)
selectAction se | VP.sum actionArity == 0 = return Nothing
                | otherwise = do
  map <- ask
  oldEntry <- lift $ HT.lookup map se

  case oldEntry of
    Just x -> return (Just x)
    _ -> do
      choice <- V.maximumBy (compare `on` transitionScore) <$> V.imapM onAction (VP.convert actionArity)
      lift $ HT.insert map se choice
      return (Just choice)

  where
    actionArity = availableActions se
    msd = meanSlotDeviation se

    onAction idx 0 = return $ StateTransition (fromIntegral idx) 0
    onAction idx arity = do
      let action = defaultActions V.! idx
      let gain = fromIntegral $ powerGain action

      outcomes <- forM (M.assocs $ pdf action) $ \(slot, prob) -> do
        let slotIdx = fromEnum slot

        let (newSe, reward) = updateStateEntry se slot idx

        st <- maybe (StateTransition 0 0) id <$> selectAction newSe

        let restReward = transitionScore st
        return (prob * (reward + restReward))
    
      let value = sum outcomes
      return $ StateTransition (fromIntegral idx) value

updateStateEntry :: StateEntry -> Slot -> Int -> (StateEntry, Double)
updateStateEntry se slot idx = (StateEntry { availableActions = newActionState, meanSlotDeviation = newMsd }, reward)
  where
    action = defaultActions V.! idx
    gain = fromIntegral $ powerGain action

    actionArity = availableActions se
    arity = actionArity VP.! idx
    newActionState = actionArity `VP.unsafeUpd` [(idx, arity-1)]
    
    slotIdx = fromEnum slot
    msd = meanSlotDeviation se
    oldSlot = msd VP.! slotIdx
    newGain = max oldSlot gain

    reward = fromIntegral (newGain - oldSlot)

    newMsd' = msd `VP.unsafeUpd` [ (slotIdx, max (msd VP.! slotIdx) gain) ]
    mean = (VP.sum newMsd') `div` 8
    newMsd = VP.map (\n -> fromIntegral $ max (0 :: Int) (fromIntegral n - fromIntegral mean)) newMsd'


-- Initialization stuff



compositions :: Int -> Int -> [[Int]]
compositions n 0 = [[]]
compositions n k = do
  tails <- compositions n (k-1)
  x <- [0..n - sum tails]
  let result = x : tails
  guard (sum result < n)
  return result


rankedActions :: VP.Vector Word8 -> [[VP.Vector Word8]]
rankedActions xs = go 1
  where
    xs' = map fromIntegral $ VP.toList xs
    len = VP.length xs 
    go n 
      | sum xs' == n = [[xs]]
      | otherwise = mkRank n : go (n+1) 
    
    mkRank n = map (VP.fromList . map fromIntegral) $ filter (and . zipWith (>=) xs') $ filter ((==n) . sum) $ compositions (n+1) len

      
buildStates :: VP.Vector Word8 -> ReaderT MDPState IO ()
buildStates acts = mapM_ (\m -> selectAction (StateEntry { meanSlotDeviation = VP.fromList (map fromIntegral m) , availableActions = acts})) (compositions 8 8)


