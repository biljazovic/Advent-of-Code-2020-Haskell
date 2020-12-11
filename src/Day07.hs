module Day07 (main07) where

import Util
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Inductive.Query (reachable)
import Data.Graph.Inductive.PatriciaTree

type Bag = String
type IT = [(Bag, [(Int, Bag)])]

parseLine :: String -> (Bag, [(Int, Bag)])
parseLine line = (topbag, bags) where
    (topbag : bags' : _) = splitOn " bags contain " line
    bags = if bags' == "no other bags." 
              then [] 
              else map f $ splitOn ", " bags'
    f bag' = let amount : (unwords . init -> bag) = words bag'
              in (read amount, bag)

makeGraph :: IT -> (Gr Bag Int, Map Bag Int)
makeGraph input = (mkGraph nodes edges, indexMap)
    where
        allBags = Set.toList $ Set.fromList $ map fst input ++ concatMap (map snd . snd) input
        indexMap = Map.fromList $ map (\(a, b) -> (b, a)) nodes
        nodes = zip [1..] allBags
        edges = concatMap makeEdges input
        makeEdges (topbag, bags) = map (\(amount, bag) -> (indexMap Map.! topbag, indexMap Map.! bag, amount)) bags


solveA :: (Gr Bag Int, Map Bag Int) -> Int
solveA (graph, (Map.! "shiny gold") -> shinyGold) = (\x -> x - 1) . length . reachable shinyGold . grev $ graph

solveB :: (Gr Bag Int, Map Bag Int) -> Int
solveB (graph, (Map.! "shiny gold") -> shinyGold) = rek shinyGold
    where
        rek n = let (_, _, _, outs) = context graph n
                 in sum (map (\(b, c) -> b * (rek c + 1)) outs)

main07 :: IO ()
main07 = do
    input <- map parseLine . filter (not . emptyLine) . lines <$> readFile "res/input07"
    let (graph, indexMap) = makeGraph input
    print $ solveA (graph, indexMap)
    print $ solveB (graph, indexMap)
