{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Binding where

import Name

data Binding f term
  = Variable Name
  | Abstraction Name term
  | Expression (f term)
  deriving (Show, Eq, Functor, Foldable)
