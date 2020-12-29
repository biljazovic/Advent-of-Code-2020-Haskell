module Day15 (main15) where

import qualified Data.IntMap as IntMap
import Util

part :: [Int] -> Int -> Int
part start index =
  if index <= length start
    then start !! (index -1)
    else rek (IntMap.fromList $ zip (init start) [0 ..]) (last start) (length start)
  where
    rek lastSeen lastSpoken numSpoken =
      if numSpoken == index
        then lastSpoken
        else
          let j = IntMap.findWithDefault (numSpoken -1) lastSpoken lastSeen
              nowSpoken = (numSpoken -1) - j
              newLastSeen = IntMap.insert lastSpoken (numSpoken -1) lastSeen
           in rek newLastSeen nowSpoken (numSpoken + 1)

main15 :: IO ()
main15 = do
  input <- map read . splitOn "," . head . lines <$> readFile "res/input15"
  print $ part input 2020
  print $ part input 30000000
