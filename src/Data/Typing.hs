module Data.Typing where

data Typing f term
  = Type Int
  | Annotation term term
  deriving (Show, Eq)
