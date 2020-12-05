module Util (module Linear.V2, module Control.Lens, module Text.Megaparsec, module Text.Megaparsec.Char, module Text.Megaparsec.Char.Lexer, module Data.Void, parseMatrix, CharMatrix, sepBy1_, module Data.List) where

import Linear.V2
import Data.Array
import Control.Lens hiding (noneOf, uncons)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.Void ( Void )
import Data.List

type CharMatrix = Array (V2 Int) Char

parseMatrix :: String -> CharMatrix
parseMatrix str =
  let strs = lines str
      n = length strs
      m = length $ head strs
      lst = concat $ [[(V2 i j, ch) | (ch, j) <- zip str [0 ..]] | (str, i) <- zip strs [0 ..]]
   in array (V2 0 0, V2 (n-1) (m-1)) lst

sepBy1_ :: MonadParsec e s m => m a -> m sep -> m [a]
sepBy1_ p sep = (:) <$> try p <*> many (try (sep *> p))
