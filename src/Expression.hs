{-# LANGUAGE DeriveFunctor #-}

module Expression where

data Expression f = Type Int
  deriving Functor
