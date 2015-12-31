{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Data.Typing where

data Typing f term
  = Type Int
  | Annotation term term
  | Binding (f term)
  deriving (Show, Eq, Functor, Foldable)
