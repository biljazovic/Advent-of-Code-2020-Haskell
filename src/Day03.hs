module Day03 (main03) where

import Data.Array

type IT = Array (Int, Int) Char

parse :: [String] -> IT
parse strs =
  let n = length strs
      m = length $ head strs
      lst = concat $ [[((i, j), ch) | (ch, j) <- zip str [0 ..]] | (str, i) <- zip strs [0 ..]]
   in array ((0, 0), (n -1, m -1)) lst

solve :: IT -> (Int, Int) -> Int
solve mat (di, dj) = length . filter (== '#') . map (mat !) . takeWhile ((< n) . fst) $ iterate f (0, 0) where
    f (i, j) = (i+di, (j+dj) `mod` m)
    (_, (n', m')) = bounds mat
    (n, m) = (n' + 1, m' + 1)

solveA :: IT -> Int
solveA = flip solve (1, 3)

solveB :: IT -> Int
solveB mat = product . map (solve mat) $ [(1,1), (1,3), (1,5), (1,7), (2,1)]

main03 :: IO ()
main03 = do
    input <- parse . lines <$> readFile "res/input03"
    print $ solveA input
    print $ solveB input