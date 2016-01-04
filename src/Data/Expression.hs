{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.Expression where

import Data.Unification

data Expression recur
  = Application recur recur
  | Lambda recur recur
  deriving (Functor, Show, Eq, Foldable, Traversable)

instance Unifiable Expression where
  unifyBy f expected actual = case (expected, actual) of
    (Application a1 b1, Application a2 b2) -> Just $ Application (f a1 a2) (f b1 b2)
    (Lambda a1 b1, Lambda a2 b2) -> Just $ Lambda (f a1 a2) (f b1 b2)
    _ -> Nothing
