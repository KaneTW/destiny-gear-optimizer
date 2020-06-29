module Destiny.Drops where

import Destiny.Types

currentLevel :: SlotTable -> Int
currentLevel slots = floor (x/n)
  where
    x = fromIntegral $ sum slots
    n = fromIntegral $ length slots

flatten :: SlotTable -> SlotTable
flatten slots = fmap (max current) slots
  where
    current = currentLevel slots

fullFlatten :: SlotTable -> SlotTable
fullFlatten = until (\st -> st == flatten st) flatten

