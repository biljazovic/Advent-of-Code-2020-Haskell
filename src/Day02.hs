module Day02 (main02) where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.Ix (inRange)

type IT = [((Int, Int), Char, String)]

type Parser = Parsec Void String

parser :: Parser IT
parser = many $ do
    bounds <- (,) <$> decimal <* char '-' <*> decimal
    ch     <- space *> lowerChar
    str    <- string ": " *> many lowerChar
    many newline
    pure (bounds, ch, str)

solveA :: IT -> Int
solveA = length . filter f where
    f (bounds, ch, str) = inRange bounds $ length (filter (== ch) str)

solveB :: IT -> Int
solveB = length . filter f where
    f ((a, b), ch, str) = length (filter (== ch) [str!!(a-1), str!!(b-1)]) == 1

main02 :: IO ()
main02 = do
    input <- parseMaybe parser <$> readFile "res/input02"
    print $ solveA <$> input
    print $ solveB <$> input
