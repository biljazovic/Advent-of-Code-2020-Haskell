module Day10 (main10) where

import Util
import qualified Data.Map.Strict as Map

type IT = [Integer]

solveA :: IT -> Int
solveA lst = listCount (== 1) diffs * listCount (== 3) diffs
  where
    diffs = zipWith (-) (tail lst) lst

solveB :: IT -> Int
solveB lst = foldl' f (Map.singleton (head lst) 1) (tail lst) Map.! toe lst
  where
    f mapa x = let v = sum $ map (\d -> Map.findWithDefault 0 (x-d) mapa) [1..3]
                in Map.insert x v mapa

main10 :: IO ()
main10 = do
    input <- map read . filter (not . emptyLine) . lines <$> readFile "res/input10"
    let lst = sort $ input ++ [0,maximum input + 3]
    print $ solveA lst
    print $ solveB lst
