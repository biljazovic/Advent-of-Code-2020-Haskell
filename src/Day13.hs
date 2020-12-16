module Day13 (main13) where

import Util
import Data.Maybe (catMaybes, isJust)

type IT = (Integer, [Maybe Integer])

parseIT :: String -> IT
parseIT str = let (x1 : x2s : _) = lines str
                  f ('x' : _) = Nothing
                  f x2 = Just $ read x2
               in (read x1, map f $ splitOn "," x2s)

solveA :: IT -> Integer
solveA (p, catMaybes -> bs) = (bs !! tko) * (opt - p)
  where
    opt = minimum diffs
    tko = argmin diffs
    diffs = map f bs
    f b = let (d, m) = p `divMod` b
           in if m == 0
                 then p
                 else (d+1)*b

euclid :: Integer -> Integer -> (Integer, Integer)
euclid a b = rek (a, b) (1, 0) (0, 1)
  where
    rek (r0, r1) (s0, s1) (t0, t1) = 
      if r1 == 0
         then (s0, t0)
         else let q = r0 `div` r1
               in rek (r1, r0 - q * r1) (s1, s0 - q * s1) (t1, t0 - q * t1)

solveB :: IT -> Integer
solveB (_, bs) = (x + (abs x `div` n + 1) * n) `mod` n
  where
    xs = map (\(i, Just x) -> (i, x)) . filter (isJust . snd) $ zip [0..] bs
    n = product $ map snd xs
    x = sum [-i * fst (euclid (n `div` ni) ni) * (n `div` ni) | (i, ni) <- xs]

main13 :: IO ()
main13 = do
    input <- parseIT <$> readFile "res/input13"
    print $ solveA input
    print $ solveB input
