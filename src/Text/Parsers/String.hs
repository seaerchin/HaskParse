module Text.Parsers.String where

import Lib
import Text.Combinators.Base
import Text.Combinators.List
import Text.Parsers.Base

parseString :: String -> Parser String
parseString s =
  map parseChar s
    |> foldr f (return "")
  where
    f :: Parser a -> Parser [a] -> Parser [a]
    f cur acc = cur `andThen` acc |> fmap asSingleParser
      where
        asSingleParser (a, b) = a : b

parseInteger :: Parser Int
parseInteger = many1 anyDigit |> fmap read
