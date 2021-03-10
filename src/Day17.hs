module Day17 (main17) where

import Util (parseMatrix, CharMatrix)
import Linear.V2
import qualified Data.Array as Arr
import Data.List ((\\))
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

type Point = [Int]
type Pocket = Set Point

susedi_ :: [Int] -> Point -> [Point]
susedi_ ds p = ps \\ [p]
  where
    ps = forM p (\x -> map (+x) ds)

susedi = susedi_ [-1..1]

numActivated :: Pocket -> Point -> Int
numActivated pocket point = length 
  $ filter (`Set.member` pocket) 
  $ susedi point

expandEmpty :: Pocket -> [(Point, Char)]
expandEmpty pocket = [(x, toChar x) | x <- Set.toList allPoints]
  where
    allPoints = Set.union pocket (Set.fromList $ concatMap susedi $ Set.elems pocket)
    toChar x = if x `Set.member` pocket then '#' else '.'

iteratePocket :: Pocket -> Pocket
iteratePocket pocket = Set.fromList . map fst . filter ((== '#') . snd) . map convert $ tPocket
  where
    tPocket = expandEmpty pocket
    convert (i, c) = let n  = numActivated pocket i
                         c' = if | c == '#' && (n < 2 || n > 3) -> '.'
                                 | c == '.' && n == 3           -> '#'
                                 | otherwise                    -> c
                      in (i, c')

solve :: CharMatrix -> Int -> Int -> Int
solve mat dim iters = Set.size $ iterate iteratePocket initialPocket !! iters
  where
    initialPocket = Set.fromList [expandWithZero i | (i, e) <- Arr.assocs mat, e == '#']
    expandWithZero (V2 x y) = [x, y] ++ replicate (dim-2) 0

main17 :: IO ()
main17 = do
    input <- parseMatrix <$> readFile "res/input17"
    print $ solve input 3 6
    print $ solve input 4 6
