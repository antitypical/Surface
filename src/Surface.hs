{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Surface (
  module Surface',
  lambda,
  Surface.pi,
  (-->),
  apply,
  checkHasFunctionType,
) where

import Prelude hiding (pi)
import Data.Binding as Surface'
import Data.Expression as Surface'
import Data.Name as Surface'
import Data.Name.Internal
import Data.Term as Surface'
import Data.Typing as Surface'
import Data.Unification as Surface'
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Construct a lambda from a type and a function from an argument variable to the resulting term. The variable will be picked automatically. The parameter type will be checked against `Type`, but there are no constraints on the type of the result.
lambda :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
lambda t f = checkedExpression type' $ Lambda t body
  where body = typedAbstraction name t scope
        scope = f $ variable name
        name = maybe (Local 0) prime $ maxBoundVariable scope
        type' context = do
          _ <- checkIsType t context
          body' <- typeOf body (Map.insert name t context)
          return $ t `pi` const body'

-- | Construct a pi type from a type and a function from an argument variable to the resulting type. The variable will be picked automatically. The parameter type will be checked against `Type`, as will the substitution of the parameter type into the body.
pi :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
pi t f = checkedExpression type' $ Lambda t body
  where body = typedAbstraction name t scope
        scope = f $ variable name
        name = maybe (Local 0) prime $ maxBoundVariable scope
        type' context = do
          _ <- checkIsType t context
          body' <- checkIsType body (Map.insert name t context)
          return $ t `pi` const body'

infixr -->

-- | Construct a non-dependent function type between two types. Both operands will be checked against `Type`.
(-->) :: Term Expression -> Term Expression -> Term Expression
a --> b = checkedExpression type' $ Lambda a b
  where type' context = do
          a' <- checkIsType a context
          b' <- checkIsType b context
          return $ a' --> b'

-- | Construct the application of one term to another. The first argument will be checked as a function type, and the second will be checked against that type’s domain. The resulting type will be the substitution of the domain type into the body.
apply :: Term Expression -> Term Expression -> Term Expression
apply a b = checkedExpression type' $ Application a b
  where type' context = do
          (type', body) <- checkHasFunctionType a context
          _ <- check type' b context
          return $ applySubstitution type' body


checkHasFunctionType :: Term Expression -> Context Expression -> Result (Term Expression, Term Expression)
checkHasFunctionType term context = do
  (Binding (Expression (Lambda type' body))) <- out <$> checkIsType term context
  return (type', body)


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

    Implicit -> ("_", maxBound)
    Annotation (_, term) (_, type') -> (wrap 3 (<=) term ++ " : " ++ wrap 3 (<) type', 3)

    :: (String, Int))
    where wrap i op (s, j) | i `op` j = s
          wrap _ _ (s, _) = "(" ++ s ++ ")"
