module Text.Combinators.List where

import Data.List.NonEmpty
import Lib
import Text.Combinators.Base (andThen, optional, orElse)
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

-- matches a given character 0 or more times
parseZeroOrMore :: Parser a -> String -> ([a], String)
parseZeroOrMore parser input =
  let parseResult = runParser parser input
   in case parseResult of
        Left s -> (mempty, input)
        Right (result, remaining) ->
          let (res, rem) = parseZeroOrMore parser remaining
           in (result : res, rem)

-- matches a given character 0 or more times
many :: Parser a -> Parser [a]
many parser = Parser (Right . parseZeroOrMore parser)

-- matches a given character at least once
many1 :: Parser a -> Parser [a]
many1 parser =
  Parser
    ( \input ->
        runParser parser input
          >>= \(res, rem) ->
            let (result, remaining) = parseZeroOrMore parser input
             in Right (res : result, remaining)
    )

-- Parses a string which is separated by 0 or more separators
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep parser = many $ optional sep >> parser

sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep parser =
  let parseManySep = many $ sep >> parser
   in (:) <$> parser <*> parseManySep