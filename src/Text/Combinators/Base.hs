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
