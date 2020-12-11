module Day08 (main08) where

import Data.Either (rights)
import qualified Data.Set as Set
import Util

type IT = Array Int Ins

data Ins = Nop Integer | Jmp Integer | Acc Integer
  deriving (Show)

readInteger :: String -> Integer
readInteger ('+' : numStr) = read numStr
readInteger numStr = read numStr

parseIns :: String -> Ins
parseIns str =
  let (ins : (readInteger -> num) : _) = words str
   in case ins of
        "nop" -> Nop num
        "acc" -> Acc num
        "jmp" -> Jmp num

simulate :: IT -> Either Integer Integer
simulate inss = rek 0 Set.empty 0
  where
    rek pc bio sum =
      let ins = inss ! pc
          pc' = case ins of
            Jmp x -> fromInteger x + pc
            _ -> pc + 1
          acc = case ins of
            Acc x -> x
            _ -> 0
       in if pc == length inss
            then Right sum
            else
              if pc > length inss || pc < 0 || Set.member pc bio
                then Left sum
                else rek pc' (Set.insert pc bio) (sum + acc)

solveA :: IT -> Integer
solveA inss =
  let Left sol = simulate inss
   in sol

solveB :: IT -> Integer
solveB inss = head . rights . map (simulate . ((//) inss . (: []))) . filter notAcc . assocs . fmap convert $ inss
  where
    convert ins = case ins of
      Jmp x -> Nop x
      Nop x -> Jmp x
      x -> x
    notAcc (_, ins) = case ins of
      Acc _ -> False
      _ -> True

main08 :: IO ()
main08 = do
  input <- listToArray . map parseIns . filter (not . emptyLine) . lines <$> readFile "res/input08"
  print $ solveA input
  print $ solveB input
