module Day05 (main05) where

type IT = [Int]

parseIT :: String -> IT
parseIT = map (((\(x, y) -> x * 8 + y) . decode) . splitAt 7) . lines where
    decode (str1, str2) = (decodeBinary ('F', 'B') str1, decodeBinary ('L', 'R') str2)
    decodeBinary (ch0, ch1) = foldl (\b ch -> b * 2 + if ch == ch0 then 0 else 1) 0

solveA :: IT -> Int
solveA = maximum

solveB :: IT -> [Int]
solveB lst = filter freeSeat [(minimum lst) .. (maximum lst)] where
    freeSeat id = id `notElem` lst

main05 :: IO ()
main05 = do
    input <- parseIT <$> readFile "res/input05"
    print $ solveA input
    print $ solveB input
