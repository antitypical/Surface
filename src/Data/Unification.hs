module Data.Unification where

class Unifiable f where
  unify :: f a -> f a -> Maybe (f a)
