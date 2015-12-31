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

instance Show (Term Expression) where
  show = show . out
