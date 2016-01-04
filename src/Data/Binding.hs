{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.Binding where

import Data.Name

data Binding f term
  = Variable Name
  | Abstraction Name term
  | Expression (f term)
  deriving (Show, Eq, Functor, Foldable, Traversable)
