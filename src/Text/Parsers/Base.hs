module Text.Parsers.Base where

-- parse a single char and returns the result if successful
import Control.Monad ((>=>))

parseChar :: Char -> Parser Char
parseChar c = Parser (\s -> if null s then Left "" else if c == head s then Right (c, tail s) else Left ("No such character: " ++ [c]))

newtype Parser a = Parser (String -> Either String (a, String))

runParser :: Parser a -> String -> Either String (a, String)
runParser (Parser p) = p

instance Functor Parser where
  fmap f p =
    Parser
      ( runParser p
          >=> (\(result, remaining) -> return (f result, remaining))
      )

-- Updates the error message for a given parser
updateErrorMessage :: Parser a -> String -> Parser a
updateErrorMessage p errMsg =
  Parser
    ( \s ->
        let res = runParser p s
         in case res of
              Left x -> Left errMsg
              success -> success
    )

-- after parsing, if it is successful, transform using the function embedded in f
instance Applicative Parser where
  f <*> p =
    -- attempt to parse with the parser provided. thereafter, extract value from f and parse from there
    Parser
      ( \s ->
          let result = runParser p s
           in case result of
                Left err -> Left err
                Right (parsed, remaining) ->
                  runParser f s >>= \(transform, remaining) -> Right (transform parsed, remaining)
      )
  pure a = Parser (\s -> Right (a, s))

instance Monad Parser where
  -- Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    Parser
      ( \s ->
          let result = runParser p s
           in case result of
                Left err -> Left err
                Right (parsed, remaining) ->
                  let parserB = f parsed
                   in runParser parserB remaining
      )