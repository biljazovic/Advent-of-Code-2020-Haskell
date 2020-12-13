module Day09 (main09) where

import Util

type IT = Array Int Integer

solveA :: IT -> Integer
solveA arr = arr ! ind
  where
    ind = head . filter f $ [25..]
    f i = arr ! i `notElem` [arr ! j + arr ! k | j <- [i - 25 .. i - 1], k <- [i - 25 .. i - 1], j /= k]

solveB :: IT -> Integer
solveB arr = let a = solveA arr
                 rek i j s
                   | s == a = map (arr !) [i..j]
                   | s > a && j-i > 1 = rek (i+1) j (s - arr ! i)
                   | otherwise = rek i (j+1) (s + arr ! j)
                 lst = rek 0 2 (arr ! 0 + arr ! 1)
              in minimum lst + maximum lst

main09 :: IO ()
main09 = do
    input <- listToArray . map read . filter (not . emptyLine) . lines <$> readFile "res/input09"
    print $ solveA input
    print $ solveB input
