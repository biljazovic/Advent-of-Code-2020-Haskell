module Day03 (main03) where

import Control.Lens ((^.))
import Util
import Data.Array

solve :: CharMatrix -> (Int, Int) -> Int
solve mat (di, dj) = length . filter (== '#') . map (mat !) . takeWhile ((< n) . (^._x)) $ iterate f (V2 0 0) where
    f (V2 i j) = V2 (i+di) ((j+dj) `mod` m)
    (_, V2 ((+1) -> n) ((+1) -> m)) = bounds mat

solveA :: CharMatrix -> Int
solveA = flip solve (1, 3)

solveB :: CharMatrix -> Int
solveB mat = product . map (solve mat) $ [(1,1), (1,3), (1,5), (1,7), (2,1)]

main03 :: IO ()
main03 = do
    input <- parseMatrix <$> readFile "res/input03"
    print $ solveA input
    print $ solveB input
