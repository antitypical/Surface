{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
module Data.Expression where

import qualified Data.Data as Data

data Expression recur
  = Application recur recur
  | Lambda recur recur
  deriving (Functor, Show, Eq, Foldable, Data.Typeable, Data.Data)
