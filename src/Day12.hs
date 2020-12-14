module Day12 (main12) where

import Util

type Ins = (Char, Integer)
type State = (V2 Integer, V2 Integer)

parseIns :: String -> Ins
parseIns str = (head str, read $ tail str)

rot :: Integer -> V2 Integer -> V2 Integer
rot a dir = let a' = (a + 360 * (abs a `div` 360 + 1)) `div` 90
                rotRight (V2 x y) = V2 y (-x)
                rots = iterate rotRight dir
             in rots !! fromInteger a'

simulateA :: Ins -> State -> State
simulateA (c, a) (pos, dir) = case c of
  'F' -> move dir
  'N' -> move (V2 0 1)
  'S' -> move (V2 0 (-1))
  'E' -> move (V2 1 0)
  'W' -> move (V2 (-1) 0)
  'R' -> turn a
  'L' -> turn (-a)
  where
    move dir' = (pos + scale a dir', dir)
    turn a' = (pos, rot a' dir)

simulateB :: Ins -> State -> State
simulateB (c, a) (pos, way) = case c of
  'F' -> (pos + scale a way, way)
  'N' -> moveWay (V2 0 1)
  'S' -> moveWay (V2 0 (-1))
  'E' -> moveWay (V2 1 0)
  'W' -> moveWay (V2 (-1) 0)
  'R' -> turn a
  'L' -> turn (-a)
  where
    moveWay dir = (pos, way + scale a dir)
    turn a' = (pos, rot a' way)


solve :: (Ins -> State -> State) -> State -> [Ins] -> Integer
solve simulate startState inss = abs x + abs y
  where
    (V2 x y, _) = foldl' (flip simulate) startState inss

main12 :: IO ()
main12 = do
    input <- map parseIns . filter (not . emptyLine) . lines <$> readFile "res/input12"
    print $ solve simulateA (V2 0 0, V2 1 0) input
    print $ solve simulateB (V2 0 0, V2 10 1) input
