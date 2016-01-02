{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
module Data.Binding where

import Data.Name
import qualified Data.Data as Data

data Binding f term
  = Variable Name
  | Abstraction Name term
  | Expression (f term)
  deriving (Show, Eq, Functor, Foldable, Data.Typeable, Data.Data)
