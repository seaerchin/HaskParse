module Main where

import Control.Applicative (liftA2)
import Lib
import Text.Combinators.Base
import Text.Combinators.List
import Text.Parsers.Base
import Text.Parsers.String

parseA :: Parser Char
parseA = parseChar 'a'

parserB = parseChar 'b'

data One = One | Oof

parser1 = parseChar '1'

-- TODO: test monadic parsing

combined :: Parser (Char, Char)
combined = parseA `andThen` parserB

orElsed :: Parser Char
orElsed = parseA `orElse` parserB

parseDigit :: Parser Char
parseDigit = anyDigit

parseThreeDigitsAsStr :: Parser [Char]
parseThreeDigitsAsStr = parseString "1234"

main :: IO ()
main = do
  print $ runParser parseThreeDigitsAsStr "12345"