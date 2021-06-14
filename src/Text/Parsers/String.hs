module Text.Parsers.String where

import Lib
import Text.Combinators.Base
import Text.Parsers.Base

parseString :: String -> Parser String
parseString s =
  map parseChar s
    |> foldr f (return "")

f :: Parser a -> Parser [a] -> Parser [a]
f cur acc = cur `andThen` acc |> fmap asSingleParser
  where
    asSingleParser (a, b) = a : b