module Lib where

(|>) :: a -> (a -> b) -> b
a |> b = b a

infixl 9 |>

tupleToString :: (Show a, Show b) => (a, b) -> String
tupleToString (a, b) = show a <> show b