{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Expression where

data Expression recur
  = Type Int
  | Application recur recur
  | Lambda recur recur
  deriving (Functor, Show, Eq, Foldable)
