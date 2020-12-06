module Day06 (main06) where

import Util
import qualified Data.Set as Set

type IT = [[String]]

parseIT :: String -> IT
parseIT = filter (not . null) . map lines . splitOn "\n\n"

solveA :: IT -> Int
solveA = sum . map (length . Set.fromList . concat)

solveB :: IT -> Int
solveB = sum . map (length . foldr1 Set.intersection . map Set.fromList)

main06 :: IO ()
main06 = do
    input <- parseIT <$> readFile "res/input06"
    print $ solveA input
    print $ solveB input
