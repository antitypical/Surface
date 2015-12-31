{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Data.Expression where

data Expression recur
  = Type Int
  | Application recur recur
  | Lambda recur recur
  deriving (Functor, Eq, Foldable)

instance Show f => Show (Expression f) where
  show (Type n) = "Type" ++ show n
  show (Application a b) = show a ++ " " ++ show b
  show (Lambda t body) = "λ : " ++ show t ++ " . " ++ show body
