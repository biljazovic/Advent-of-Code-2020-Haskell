module Day04 (main04, validField) where

import Util
import Data.Maybe (isJust)
import Control.Monad (guard, forM)
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Ix (inRange)

type IT = [[(String, String)]]

type Parser = Parsec Void String

parser :: Parser IT
parser = sepBy1_ passport (count 2 newline) <* many newline

passport = sepBy1_ field (oneOf " \n") where
    field = (,) <$> word <*> (char ':' *> word)
    word = some $ noneOf " \n:"

reqFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
optFields = ["cid"]
allFields = reqFields ++ optFields

solveA :: IT -> Int
solveA = length . filter f where
    f pass = all (\f -> any ((== f) . fst) pass) reqFields

validField :: String -> String -> Bool
validField field s = if | field == "byr" -> inBounds s 1920 2002
                        | field == "iyr" -> inBounds s 2010 2020
                        | field == "eyr" -> inBounds s 2020 2030
                        | field == "hgt" -> let n = length s
                                                (num, unit) = splitAt (n-2) s
                                             in unit == "cm" && inBounds num 150 193 ||
                                                unit == "in" && inBounds num 59 76
                        | field == "hcl" -> let (ch, color) = splitAt 1 s
                                             in ch == "#" && length color == 6 &&
                                                all (\c -> isDigit c || ('a', 'f') `inRange` c) color
                        | field == "ecl" -> s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                        | field == "pid" -> all isDigit s && length s == 9
                        | otherwise -> False where
                            inBounds s a b = all isDigit s && inRange (a, b) (read s :: Integer)

solveB :: IT -> Int
solveB = length . filter valid where
    valid pass = isJust $ forM reqFields $ \key -> do
        value <- lookup key pass
        guard $ validField key value

main04 :: IO ()
main04 = do
    Just input <- parseMaybe parser <$> readFile "res/input04"
    print $ solveA input
    print $ solveB input
