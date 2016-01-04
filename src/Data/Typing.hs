{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.Typing where

data Typing f term
  = Type Int
  | Annotation term term
  | Binding (f term)
  | Implicit
  deriving (Show, Eq, Functor, Foldable, Traversable)
