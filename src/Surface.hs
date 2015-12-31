{-# LANGUAGE FlexibleInstances #-}
module Surface (
  module Surface',
  lambda,
  _type,
  _type',
  apply,
) where

import Data.Binding as Surface'
import Data.Expression as Surface'
import Data.Name as Surface'
import Data.Name.Internal
import Data.Term as Surface'
import qualified Data.Maybe as Maybe

lambda :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
lambda t f = expression . Lambda t $ abstraction name body
  where body = f $ variable name
        name = Maybe.fromMaybe (Local 0) $ maxBoundVariable body

_type :: Int -> Term Expression
_type n = expression $ Type n

_type' :: Term Expression
_type' = _type 0

apply :: Term Expression -> Term Expression -> Term Expression
apply a b = expression $ Application a b

instance Show (Term Expression) where
  show = fst . cata (\ b -> case b of
    Variable n -> (show n, -1)
    Abstraction _ body -> body
    Expression (Type 0) -> ("Type", -1)
    Expression (Type n) -> ("Type" ++ showNumeral "₀₁₂₃₄₅₆₇₈₉" n, -1)
    Expression (Application a b) -> (fst a ++ " " ++ fst b, 4)
    Expression e -> (show e, -1))
    where wrap i (s, j) | i > j = s
          wrap i (s, _) = "(" ++ s ++ ")"

instance Eq (Term Expression) where
  a == b = freeVariables a == freeVariables b && out a == out b
