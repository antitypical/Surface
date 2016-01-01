{-# LANGUAGE FlexibleInstances #-}
module Surface (
  module Surface',
  lambda,
  (-->),
  apply,
) where

import Data.Binding as Surface'
import Data.Expression as Surface'
import Data.Name as Surface'
import Data.Name.Internal
import Data.Term as Surface'
import Data.Typing as Surface'
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

lambda :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
lambda t f = expression . Lambda t $ abstraction name body
  where body = f $ variable name
        name = maybe (Local 0) prime $ maxBoundVariable body

(-->) :: Term Expression -> Term Expression -> Term Expression
a --> b = expression $ Lambda a b

apply :: Term Expression -> Term Expression -> Term Expression
apply a b = expression $ Application a b


check :: Term Expression -> Term Expression -> Result (Term Expression)
check term type' = typeOf term >>= unify type'

unify :: Term Expression -> Term Expression -> Result (Term Expression)
unify expected actual = if expected == actual
  then Right expected
  else case (out expected, out actual) of
    (t, Implicit) -> Right expected
    _ -> Left $ "could not unify '" ++ show actual ++ "' with expected type '" ++ show expected ++ "'"


instance Show (Term Expression) where
  show = fst . para (\ b -> case b of
    Binding (Variable n) -> (show n, maxBound)
    Binding (Abstraction _ (_, body)) -> body

    Type 0 -> ("Type", maxBound)
    Type n -> ("Type" ++ showNumeral "₀₁₂₃₄₅₆₇₈₉" n, maxBound)

    Binding (Expression (Application (_, a) (_, b))) -> (wrap 4 (<=) a ++ " " ++ wrap 4 (<) b, 4)

    Binding (Expression (Lambda (_, type') (Term _ _ (Binding (Abstraction name (Term free _ _))), body))) | Set.member name free -> ("λ " ++ show name ++ " : " ++ wrap 3 (<) type' ++ " . " ++ wrap 3 (<=) body, 3)
    Binding (Expression (Lambda (_, type') (Term _ _ (Binding (Abstraction _ _)), body))) -> ("λ _ : " ++ wrap 3 (<) type' ++ " . " ++ wrap 3 (<=) body, 3)
    Binding (Expression (Lambda (_, type') (_, body))) -> (wrap 3 (<) type' ++ " → " ++ wrap 3 (<=) body, 3)

    :: (String, Int))
    where wrap i op (s, j) | i `op` j = s
          wrap i _ (s, _) = "(" ++ s ++ ")"

instance Eq (Term Expression) where
  a == b = freeVariables a == freeVariables b && out a == out b
