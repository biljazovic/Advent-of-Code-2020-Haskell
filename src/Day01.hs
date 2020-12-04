module Day01 (main01) where

import Data.List (find)
import Data.Maybe (mapMaybe)

type IT = [Int]

parse :: String -> IT
parse = map read . lines

solveA :: IT -> Int
solveA arr = uncurry (*) . head $ mapMaybe f arrI where
    arrI = zip arr [0..]
    f (x, i) = (x, ) . fst <$> find (\(y, j) -> i /= j && y + x == 2020) arrI

solveB :: IT -> Int
solveB arr = (\(a, b, c) -> a * b * c) . head $ lst where
    arrI = zip arr [0..]
    lst = [(a, b, c) | (a, i) <- arrI,
                       (b, j) <- arrI,
                       (c, k) <- arrI,
                       i /= j && j /= k && i /= k
                       && a + b + c == 2020]

main01 :: IO ()
main01 = do
    input <- parse <$> readFile "res/input01"
    print $ solveA input
    print $ solveB input
