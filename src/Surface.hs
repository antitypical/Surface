{-# LANGUAGE FlexibleInstances #-}
module Surface (
  module Surface,
) where

import Data.Binding as Surface
import Data.Expression as Surface
import Data.Name as Surface
import Data.Term as Surface
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
  show = show . out

instance Eq (Term Expression) where
  a == b = freeVariables a == freeVariables b && out a == out b
