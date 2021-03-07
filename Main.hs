module Main where

ack :: Integer -> Integer -> Integer -> Integer
ack x y z
 | z == 0 = y + 1
 | z == 1 = x + y 
 | z == 2 = x * y
 | z == 3 = x ^ y
 | y == 0 = case z of
                 1 -> x
                 2 -> 0
                 _ -> 1
 | otherwise = ack x (ack x (y-1) z) (z-1)

main = do
  print $ ack 5 3 2
  print $ ack 5 2 3
  print $ ack 3 5 2
  print $ ack 3 2 5
  print $ ack 2 5 3
  print $ ack 2 3 5
  

