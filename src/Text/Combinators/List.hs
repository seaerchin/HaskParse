module Text.Combinators.List where

import Data.List.NonEmpty
import Lib
import Text.Combinators.Base (orElse)
import Text.Parsers.Base
import Prelude hiding (head, map, tail)

-- tries all the parsers in the list
choice :: NonEmpty (Parser a) -> Parser a
choice (a :| []) = a
-- list with >1 elements
choice a = foldr orElse (head a) (tail a)

-- attempts to parse any character
anyChar :: Parser Char
anyChar = map parseChar (fromList ['a' .. 'z']) |> choice

-- attempts to parse any digit
anyDigit :: Parser Char
anyDigit = map parseChar (fromList ['1' .. '9']) |> choice

many :: Parser String
