module Day11 (main11) where

import Util
import Data.Array

simulate :: (CharMatrix -> CharMatrix) -> CharMatrix -> Int
simulate f mat =
  let mat' = f mat
   in if mat' == mat
        then listCount (== '#') $ elems mat
        else simulate f mat'

solveA :: CharMatrix -> Int
solveA = simulate f
  where
    f mat = let changeable = filter ((/= '.') . snd) $ assocs mat
                changed = map g changeable
                g (x, c) = let cs = map (mat !) $ susedi8 (Just (bounds mat)) x
                               occupied = listCount (== '#') cs
                               c' = if | c == '#' && occupied >= 4 -> 'L'
                                       | c == 'L' && occupied == 0 -> '#'
                                       | otherwise                 -> c
                            in (x, c')
             in mat // changed

bSusedi :: CharMatrix -> V2 Int -> [Char]
bSusedi mat x = [g (V2 i j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]
  where
    g dir =
      let kand = head $ dropWhile emptyCell (tail (iterate (+ dir) x))
          emptyCell x = inBounds (bounds mat) x && mat ! x == '.'
       in if inBounds (bounds mat) kand
            then mat ! kand
            else '.'

solveB :: CharMatrix -> Int
solveB = simulate f
  where
    f mat =
      let changeable = filter ((/= '.') . snd) $ assocs mat
          changed = map g changeable
          g (x, c) = let cs = bSusedi mat x
                         occupied = listCount (== '#') cs
                         c' = if | c == '#' && occupied >= 5 -> 'L'
                                 | c == 'L' && occupied == 0 -> '#'
                                 | otherwise -> c
                      in (x, c')
       in mat // changed

main11 :: IO ()
main11 = do
    input <- parseMatrix <$> readFile "res/input11"
    print $ solveA input
    print $ solveB input
