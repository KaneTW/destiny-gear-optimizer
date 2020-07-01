{-# LANGUAGE Strict #-}
module Main where

import Control.DeepSeq

import Control.Monad
import Control.Monad.State.Strict as State

import qualified Data.ByteString as BL
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Data.Store

import Data.Word

import Destiny.Drops
import Destiny.Types

import System.Console.Haskeline
import System.Directory

import Text.Read
import Text.Printf

import Debug.Trace

cands :: Int -> Int -> [[Int]]
cands n 0 = [[]]
cands n k = do
  tails <- cands n (k-1)
  x <- [0..n - sum tails]
  let result = x : tails
  guard (sum result < n)
  return result


generateMain :: IO ()
generateMain = do
  let acts = mkInitialActions defaultActions
  s <- execStateT (mapM_ (\m -> selectAction (StateEntry { meanSlotDeviation = VU.fromList (map fromIntegral m) , availableActions = acts}) >> liftIO (print m)) (cands 8 8)) mempty
  BL.writeFile "states.out" $ encode $ HM.toList s

main :: IO ()
main = runInputT defaultSettings (prepare >>= loopOuter)
  where
    waitForFilename = do
      fn <- getInputLine "Enter filename: "
      case fn of
        Nothing -> waitForFilename
        Just x -> return x
        
    prepare = do
      ex <- liftIO $ doesFileExist "states.out"
      states <- if ex then outputStrLn "Reading states.out..." >> processInput "states.out"
      else do
        outputStrLn "Could not find states.out."
        outputStrLn "Would you like to 1) generate it [~30 mins], 2) run without it [slow] or 3) supply a file path?"
        input <- getInputLine "[1-3]: "
        case input of
          Just "1" -> do 
            outputStrLn "Generating... this will take a long while"
            liftIO generateMain
            processInput "states.out"
          Just "2" -> return mempty
          Just "3" -> waitForFilename >>= processInput
          _ -> outputStrLn "Invalid input." >> prepare
      

      return states

    processInput :: FilePath -> InputT IO MDPState
    processInput fn = liftIO $  BL.readFile fn >>= decodeIO

    loopOuter :: MDPState -> InputT IO ()
    loopOuter states = do
      outputStrLn "States loaded. Please enter your light level."
      result <- forM ([minBound..maxBound] :: [Slot]) querySlot
      let slotTable = M.fromList result

      loop (states, mkState (fullFlatten slotTable) defaultActions, 0)

      loopOuter states

    loop :: (MDPState, StateEntry, Double) -> InputT IO ()
    loop (states, se, cg) = do
      outputStrLn $ printf "Current gain: %.2f" cg
      (result, s) <- runStateT (selectAction se) states
      case result of
        Nothing -> outputStrLn "Nothing can be improved anymore."
        Just (actIdx,gain) -> do 
          let recAction = defaultActions V.! actIdx
          outputStrLn $ printf "Do one of %s for an expected total gain of %.2f." (show $ names recAction) gain
          
          outputStrLn "What slot did you get a drop in?"
          slot <- queryUpdate (M.keys $ pdf recAction)
          let (newSe, reward) = updateStateEntry se slot actIdx
          
          loop (s, newSe, cg + reward)

    querySlot slot = do
      line <- getInputLine $ show slot ++ ": "
      case (line >>= readMaybe) of
        Nothing -> outputStrLn "Invalid input..." >> querySlot slot
        Just n -> return (slot, n)

    queryUpdate slots = do
      forM_ slots $ \slot -> do
        outputStrLn $ printf "%d) %s" (fromEnum slot + 1) (show slot)
      line <- getInputLine "[1..8]: "
      case (line >>= readMaybe) of
        Nothing -> queryUpdate slots
        Just n -> do
          let selected = toEnum (n-1)
          if selected `elem` slots then return selected
          else outputStrLn "Invalid slot." >> queryUpdate slots
