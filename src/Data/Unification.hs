module Data.Unification where

class Unifiable a where
  unify :: a -> a -> Maybe a
