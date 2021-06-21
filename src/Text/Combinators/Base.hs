module Text.Combinators.Base where

-- and then combinator
-- runs the first parser then the second

import Control.Monad ((>=>))
import Text.Parsers.Base

-- combines the result of two parsers sequentially
andThen :: Parser a -> Parser b -> Parser (a, b)
andThen first second =
  Parser
    ( runParser first
        >=> \(result, remaining) ->
          runParser second remaining
            >>= (\(secondResult, final) -> return ((result, secondResult), final))
    )

-- tries the first parser and if it fails, uses the second parser
orElse :: Parser a -> Parser a -> Parser a
orElse first second =
  let combined s =
        case runParser first s of
          Right x -> Right x
          _ -> runParser second s
   in Parser combined

-- tries the given parser at most once
optional :: Parser a -> Parser (Maybe a)
optional parser =
  Parser
    ( \input ->
        let parserResult = runParser parser input
         in case parserResult of
              Left s -> Right (Nothing, input)
              Right (res, rem) -> Right (Just res, rem)
    )

-- equivalent to >>
ignore :: Parser a -> Parser b -> Parser b
ignore = (>>)

(<<) :: Parser a -> Parser b -> Parser a
a << b = fmap fst $ a `andThen` b

between :: Parser a -> Parser b -> Parser c -> Parser b
between left payload right = left >> payload << right