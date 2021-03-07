module Day15 (main15) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import qualified Data.Vector.Unboxed.Mutable as Vec
import Util

partTransfomers :: [Int] -> Int -> Int
partTransfomers start index = runST $ evalStateT (stat start index) 0

stat :: [Int] -> Int -> StateT Int (ST s) Int
stat start index = do
  lastSeen <- lift $ Vec.replicate (index + 10) (-1)
  forM_ (zip (init start) [0 ..]) $ \(s, i) -> do
    lift $ Vec.unsafeWrite lastSeen s i
  put $ last start
  forM_ [length start .. (index - 1)] $ \numSpoken -> do
    lastSpoken <- get
    x <- lift $ Vec.unsafeRead lastSeen lastSpoken
    let newSpoken =
          if x == -1
            then 0
            else (numSpoken - 1) - x
    lift $ Vec.unsafeWrite lastSeen lastSpoken (numSpoken - 1)
    put newSpoken
  get

main15 :: IO ()
main15 = do
  input <- map read . splitOn "," . head . lines <$> readFile "res/input15"
  print $ partTransfomers input 2020
  print $ partTransfomers input 30000000
