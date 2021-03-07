module Day16 (main16) where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.Ix (inRange)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.MaxFlow
import Data.Maybe (fromJust)

data Rule = Rule { ruleName :: String, ruleBound :: [(Int, Int)] }
  deriving Show
newtype Ticket = Ticket { ticketNums :: [Int] }
  deriving Show

data IT = IT [Rule] Ticket [Ticket]
  deriving Show

type Parser = Parsec Void String

parser :: Parser IT
parser = do
  rules <- flip manyTill (string "your ticket:" *> many spaceChar) $ do
    name <- manyTill anySingle (string ": ")
    bounds <- ((,) <$> decimal <* char '-' <*> decimal) `sepBy` string " or "
    many spaceChar
    return $ Rule name bounds
  let parseCommaSeparatedValues = (decimal `sepBy1` char ',') <* newline
  myTicket <- parseCommaSeparatedValues <* newline
  manyTill anySingle newline
  nearbyTickets <- many parseCommaSeparatedValues
  many spaceChar
  pure $ IT rules (Ticket myTicket) (map Ticket nearbyTickets)

matchRule :: Int -> Rule -> Bool
matchRule x (Rule _ bound) = any (`inRange` x) bound

solveA :: IT -> Int
solveA (IT rules _ (concatMap ticketNums -> nearbyTickets)) =
  let good x = any (matchRule x) rules
   in sum $ filter (not . good) nearbyTickets

--solveB :: IT -> Int
solveB (IT rules (Ticket myTicket) nearbyTickets) = 
  product $ map (myTicket !!) importantIndices
  where
    goodInt x = any (matchRule x) rules
    goodTicket (Ticket ints) = all goodInt ints
    goodNearbyTickets = filter goodTicket nearbyTickets

    n = length rules

    nodes = map (,()) $ [-2, -1] ++ [0..n-1] ++ [n..2*n-1]
    goodEdge (i, j) = all 
      ((`matchRule` (rules !! j)) 
        . (\(Ticket ints) -> ints !! i)) 
      goodNearbyTickets
    edges = concat [[(-2, i, 1), (i+n, -1, 1)] | i <- [0..n-1]]
              ++ map (\(i, j) -> (i, j+n, 1)) (filter goodEdge [(i, j) | i <- [0..n-1], j <- [0..n-1]])
    graph = mkGraph nodes edges :: Gr () Int

    mf = maxFlowgraph graph (-2) (-1)
    pairings = map (\(i, j, _) -> (j-n, i)) . 
      filter (\(i, j, (d, _)) -> d == 1 && i >= 0 && j >= 0) $ labEdges mf
    importantRules = filter (("departure" `isPrefixOf`) . ruleName . (rules !!)) [0..n-1]
    importantIndices = map (fromJust . flip lookup pairings) importantRules

main16 :: IO ()
main16 = do
    Just input <- parseMaybe parser <$> readFile "res/input16"
    print $ solveA input
    print $ solveB input
