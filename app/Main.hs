{-# LANGUAGE Strict #-}
module Main where

import Codec.Compression.Lzma

import Control.DeepSeq

import Control.Monad
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Monad.Parallel as MP
import Control.Concurrent.STM
import qualified Data.HashTable as HT

import Data.Store

import Data.Word

import Destiny.Drops
import Destiny.Types

import System.Console.Haskeline
import System.Directory

import Text.Read
import Text.Printf

buildAllStatesParallel :: InputT IO MDPState
buildAllStatesParallel = do
  ht <- liftIO $ HT.newWithDefaults (6435 * actionSize defaultActions)
  let ranks = rankedActions $ mkInitialActions defaultActions
  let total = sum (map length ranks)
  progress <- liftIO $ newTVarIO 0
  forM_ ranks $ \acts -> do
    -- acts are bottom-up from the same rank
    liftIO $ forConcurrently_  acts (\act -> forkIO $ runReaderT (buildStates act) ht)
    previousProgress <- liftIO $ readTVarIO progress
    let newProgress = previousProgress + length acts
    outputStrLn $ printf "Progress: %.2f%%" ( (100 :: Double) * (fromIntegral newProgress) / (fromIntegral total))
    liftIO $ atomically $ writeTVar progress newProgress
  size <- liftIO $ HT.readSizeIO ht
  load <- liftIO $ HT.readLoad ht
  outputStrLn $ printf "Size: %d, Load: %d" size load
  return ht

generateMain :: InputT IO ()
generateMain = do
  mdpState <- buildAllStatesParallel
  error "abort"
  encoded <- liftIO $ encode <$> HT.readAssocsIO  mdpState
  --let compressed = compressWith defaultCompressParams { compressLevel = CompressionLevel9, compressIntegrityCheck = IntegrityCheckSha256 } (BL.fromStrict encoded)
  liftIO $ BS.writeFile "states.out" $ encoded

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
            generateMain
            processInput "states.out"
          Just "2" -> liftIO $ HT.newWithDefaults 1
          Just "3" -> waitForFilename >>= processInput
          _ -> outputStrLn "Invalid input." >> prepare
      

      return states

    processInput :: FilePath -> InputT IO MDPState
    --processInput fn = liftIO $ BL.readFile fn >>= (decodeIO . BL.toStrict . decompress) >>= error "welp"
    processInput fn = error "welp"

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
      result <- liftIO $ runReaderT (selectAction se) states
      case result of
        Nothing -> outputStrLn "Nothing can be improved anymore."
        Just (StateTransition actIdx' gain) -> do 
          let actIdx = fromIntegral actIdx'
          
          let recAction = defaultActions V.! actIdx
          outputStrLn $ printf "Do one of %s for an expected total gain of %.2f." (show $ names recAction) gain
          
          outputStrLn "What slot did you get a drop in?"
          slot <- queryUpdate (M.keys $ pdf recAction)
          let (newSe, reward) = updateStateEntry se slot actIdx
          
          loop (states, newSe, cg + reward)

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
