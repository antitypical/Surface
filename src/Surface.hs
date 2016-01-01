{-# LANGUAGE FlexibleInstances #-}
module Surface (
  module Surface',
  lambda,
  Surface.pi,
  (-->),
  apply,
  annotation,
  checkHasFunctionType,
) where

import Data.Binding as Surface'
import Data.Expression as Surface'
import Data.Name as Surface'
import Data.Name.Internal
import Data.Term as Surface'
import Data.Typing as Surface'
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Construct a lambda from a type and a function from an argument variable to the resulting term. The variable will be picked automatically. The parameter type will be checked against `Type`, but there are no constraints on the type of the result.
lambda :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
lambda t f = checkedExpression type' $ Lambda t body
  where body = abstraction name scope
        scope = f $ variable name
        name = maybe (Local 0) prime $ maxBoundVariable scope
        type' context = do
          body' <- typeOf body (Map.insert name t context)
          return $ t --> body'

-- | Construct a pi type from a type and a function from an argument variable to the resulting type. The variable will be picked automatically. The parameter type will be checked against `Type`, as will the substitution of the parameter type into the body.
pi :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
pi t f = checkedExpression type' $ Lambda t body
  where body = abstraction name scope
        scope = f $ variable name
        name = maybe (Local 0) prime $ maxBoundVariable scope
        type' context = do
          body' <- checkIsType body (Map.insert name t context)
          return $ t --> body'

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
          operand <- check type' b context
          return $ applySubstitution type' body

-- | Construct the annotation of a term by a type. The term will be checked against this type.
annotation :: Foldable f => Term f -> Term f -> Term f
annotation term type' = checkedTyping (check type' term) $ Annotation term type'


unify :: Term Expression -> Term Expression -> Result (Term Expression)
unify expected actual = if expected == actual
  then Right expected
  else case (out expected, out actual) of
    (t, Implicit) -> Right expected
    (Binding (Expression (Application a1 b1)), Binding (Expression (Application a2 b2))) -> do
      a <- unify a1 a2
      b <- unify b1 b2
      return $ apply a b
    (Binding (Expression (Lambda type1 body1)), Binding (Expression (Lambda type2 body2))) -> do
      type' <- unify type1 type2
      body <- unify body1 body2
      let metatype context = do
            metatype1 <- typeOf expected context
            metatype2 <- typeOf actual context
            unify metatype1 metatype2
      return $ checkedExpression metatype $ Lambda type' body
    (Binding (Abstraction name1 scope1), Binding (Abstraction name2 scope2)) -> do
      let name = pick (freeVariables expected `mappend` freeVariables actual)
      scope <- unify (rename name1 name scope1) (rename name2 name scope2)
      return $ abstraction name scope
    _ -> Left $ "could not unify '" ++ show actual ++ "' with expected type '" ++ show expected ++ "'"


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

    :: (String, Int))
    where wrap i op (s, j) | i `op` j = s
          wrap i _ (s, _) = "(" ++ s ++ ")"

instance Eq (Term Expression) where
  a == b = freeVariables a == freeVariables b && out a == out b
