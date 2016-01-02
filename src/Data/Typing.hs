{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
module Data.Typing where

import qualified Data.Data as Data

data Typing f term
  = Type Int
  | Annotation term term
  | Binding (f term)
  | Implicit
  deriving (Show, Eq, Functor, Foldable, Data.Typeable, Data.Data)
