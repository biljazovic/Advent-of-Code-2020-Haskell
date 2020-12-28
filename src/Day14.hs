{-# LANGUAGE QuasiQuotes #-}

module Day14 (main14) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Digits (digits, unDigits)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Text.Scanf
import Util

data Ins = Mask String | Mem Integer Integer
  deriving (Show, Eq)

type State = (String, Map Integer Integer)

parseIns :: String -> Maybe Ins
parseIns str = mask <|> mem
  where
    mask = do
      (x :+ ()) <- scanf [fmt|mask = %s|] str
      return $ Mask x
    mem = do
      (x :+ (y :+ ())) <- scanf [fmt|mem[%l] = %l|] str
      return $ Mem x y

stepA :: Ins -> State -> State
stepA ins (mask, regs) =
  case ins of
    Mask newmask -> (newmask, regs)
    Mem reg value ->
      let dgts = digits 2 value
          combineDigits m x = case m of
            'X' -> x
            '1' -> 1
            '0' -> 0
          finalDgts = reverse $ zipWith combineDigits (reverse mask) (reverse dgts ++ repeat 0)
          finalValue = unDigits 2 finalDgts
       in (mask, Map.insert reg finalValue regs)

stepB :: Ins -> State -> State
stepB ins (mask, regs) =
  case ins of
    Mask newmask -> (newmask, regs)
    Mem reg value ->
      let dgts = digits 2 reg
          combineDigits m x = case m of
            'X' -> -1
            '1' -> 1
            '0' -> x
          finalDgts = reverse $ zipWith combineDigits (reverse mask) (reverse dgts ++ repeat 0)
          changedRegs = map (unDigits 2) $
            forM finalDgts $ \case
              (-1) -> [0, 1]
              x -> [x]
       in (mask, foldr (`Map.insert` value) regs changedRegs)

genericPart :: (Ins -> State -> State) -> [Ins] -> Integer
genericPart step = sum . Map.elems . snd . foldl' (flip step) ([], Map.empty)

main14 :: IO ()
main14 = do
  input <- mapMaybe parseIns . filter (not . emptyLine) . lines <$> readFile "res/input14"
  print $ genericPart stepA input
  print $ genericPart stepB input
