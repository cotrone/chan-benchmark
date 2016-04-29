{-# LANGUAGE KindSignatures #-}
module Main where

import qualified Data.HashMap.Strict as H
import System.Random
import Control.Monad
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import System.CPUTime
import Text.Printf

import Criterion
import Criterion.Types
import Criterion.Main
import Criterion.Measurement

import qualified Control.Concurrent.Chan as M

tests = [5000,5500..20000]

main = defaultMain [benchChan "Unagi-Chan" runUnagiChan tests, benchChan "TChan" runTChan tests, benchChan "MChan" runMChan tests]

benchChan :: String -> (Int -> IO ()) -> [Int] -> Benchmark
benchChan name channelTest tests = bgroup name $ (flip map) tests $ \t -> bench (show t) $ nfIO $ channelTest t

nRandoms :: Int -> IO [Int]
nRandoms n = replicateM n randomIO

runUnagiChan :: Int -> IO ()
runUnagiChan = runChan newChan (readChan . snd) (writeChan . fst)

runTChan :: Int -> IO ()
runTChan = runChan newTChanIO (atomically . readTChan) (\c n -> atomically $ writeTChan c n)

runMChan :: Int -> IO ()
runMChan = runChan M.newChan M.readChan M.writeChan

runChan :: IO chan -> (chan -> IO Int) -> (chan -> Int -> IO ()) -> Int -> IO ()
runChan c r w n = do
  chan <- c
  mapM_ (w chan) [1..n]
  replicateM_ n $ r chan 