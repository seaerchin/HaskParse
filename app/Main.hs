module Main where

import Control.Applicative (liftA2)
import Control.Monad
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

digitThenSemicolon = parseDigit >> optional (parseChar ';')

parseOnes = parseString "12" |> many1 |> fmap join

parseOne = parseChar '1'

parseTwo = parseChar '2'

quotedInteger = between parseOne parseDigit parseTwo

parseComma = parseChar ','

oneOrMoreDigitList = sepBy parseComma parseInt

parseInt :: Parser Int
parseInt = fmap (\x -> read [x]) parseDigit

main :: IO ()
main =
  do
    print $ runParser parseInteger "11112345"
    print $ runParser parseOnes "1212122333"
    print $ runParser digitThenSemicolon "1;asdf"
    print $ runParser quotedInteger "13242"
    print $ runParser oneOrMoreDigitList "Z;"