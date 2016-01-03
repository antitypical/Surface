{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.Expression where

import Data.Unification

data Expression recur
  = Application recur recur
  | Lambda recur recur
  deriving (Functor, Show, Eq, Foldable, Traversable)

instance Unifiable recur => Unifiable (Expression recur) where
  unify expected actual = case (expected, actual) of
    (Application a1 b1, Application a2 b2) -> do
      a <- unify a1 a2
      b <- unify b1 b2
      return $ Application a b
    (Lambda a1 b1, Lambda a2 b2) -> do
      a <- unify a1 a2
      b <- unify b1 b2
      return $ Lambda a b
    _ -> Nothing
