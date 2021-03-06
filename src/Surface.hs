{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Surface (
  module Surface',
  lambda,
  Surface.pi,
  (-->),
  apply,
  checkHasFunctionType,
  checkIsFunctionType,
  checkIsType,
) where

import Prelude hiding (pi)
import Data.Binding as Surface'
import Data.Expression as Surface'
import Data.Name as Surface'
import Data.Name.Internal
import Data.Term as Surface'
import Data.Term.Types as Surface'
import Data.Typing as Surface'
import Data.Unification as Surface'
import Control.Applicative
import qualified Data.Set as Set

infixr `lambda`

-- | Construct a lambda from a type and a function from an argument variable to the resulting term. The variable will be picked automatically. The parameter type will be checked against `Type`, but there are no constraints on the type of the result.
lambda :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
lambda t f = checkedExpression checkType $ Lambda t body
  where body = abstract f
        checkType expected context = case out expected of
          Binding (Expression (Lambda from to)) -> checkTypeAgainst from to context
          Type _ -> checkTypeAgainst _type' _type' context
          _ -> checkTypeAgainst implicit implicit context >>= expectUnifiable expected
        checkTypeAgainst from to context = do
          _ <- checkIsType t context
          _ <- expectUnifiable from t
          bodyType <- inferTypeOf body (extendContext t context body)
          _ <- expectUnifiable to bodyType
          return $ case shadowing body bodyType of
            Just name -> t `pi` \ v -> substitute name v bodyType
            _ -> t --> bodyType

infixr `pi`

-- | Construct a pi type from a type and a function from an argument variable to the resulting type. The variable will be picked automatically. The parameter type will be checked against `Type`, as will the substitution of the parameter type into the body.
pi :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
pi t f = checkedExpression checkType $ Lambda t body
  where body = abstract f
        checkType expected context = case out expected of
          Binding (Expression (Lambda from to)) -> checkTypeAgainst from to context
          Type _ -> checkTypeAgainst _type' _type' context
          _ -> checkTypeAgainst implicit implicit context >>= expectUnifiable expected
        checkTypeAgainst from to context = do
          _ <- checkIsType t context
          _ <- expectUnifiable from t
          bodyType <- checkIsType body (extendContext t context body)
          _ <- expectUnifiable to bodyType
          return $ case shadowing body bodyType of
            Just name -> t `pi` \ v -> substitute name v bodyType
            _ -> t --> bodyType

infixr -->

-- | Construct a non-dependent function type between two types. Both operands will be checked against `Type`.
(-->) :: Term Expression -> Term Expression -> Term Expression
a --> b = checkedExpression (checkInferred inferType) $ Lambda a b
  where inferType context = do
          a' <- checkIsType a context
          b' <- checkIsType b context
          return $ a' --> b'

-- | Construct the application of one term to another. The first argument will be checked as a function type, and the second will be checked against that type’s domain. The resulting type will be the substitution of the domain type into the body.
apply :: Term Expression -> Term Expression -> Term Expression
apply a b = checkedExpression (checkInferred inferType) $ Application a b
  where inferType context = do
          (type', body) <- checkHasFunctionType a context
          _ <- typeOf b type' context
          return $ applySubstitution type' body


checkHasFunctionType :: Term Expression -> Context (Term Expression) -> Result (Term Expression, Term Expression)
checkHasFunctionType term context = inferTypeOf term context >>= checkIsFunctionType

checkIsFunctionType :: Term Expression -> Result (Term Expression, Term Expression)
checkIsFunctionType type' = case out type' of
  Binding (Expression (Lambda type' body)) -> return (type', body)
  _ -> Left "Expected function type."

instance Monoid a => Alternative (Either a) where
  empty = Left mempty
  Left a <|> Left b = Left $ a `mappend` b
  Right a <|> _ = Right a
  _ <|> Right b = Right b

checkIsType :: Term Expression -> Inferer (Term Expression)
checkIsType term context = typeOf term _type' context <|> typeOf term (_type' --> _type') context


instance Show (Term Expression) where
  show = fst . para (\ b -> case b of
    Binding (Variable n) -> (show n, maxBound)
    Binding (Abstraction _ (_, body)) -> body

    Type 0 -> ("Type", maxBound)
    Type n -> ("Type" ++ showNumeral "₀₁₂₃₄₅₆₇₈₉" n, maxBound)

    Binding (Expression (Application (_, a) (_, b))) -> (wrap 4 (<=) a ++ " " ++ wrap 4 (<) b, 4)

    Binding (Expression (Lambda (_, type') (Term _ _ (Binding (Abstraction name term)), body))) | Set.member name (freeVariables term) -> ("λ " ++ show name ++ " : " ++ wrap 3 (<) type' ++ " . " ++ wrap 3 (<=) body, 3)
    Binding (Expression (Lambda (_, type') (Term _ _ (Binding (Abstraction _ _)), body))) -> ("λ _ : " ++ wrap 3 (<) type' ++ " . " ++ wrap 3 (<=) body, 3)
    Binding (Expression (Lambda (_, type') (_, body))) -> (wrap 3 (<) type' ++ " → " ++ wrap 3 (<=) body, 3)

    Implicit -> ("_", maxBound)
    Annotation (_, term) (_, type') -> (wrap 3 (<=) term ++ " : " ++ wrap 3 (<) type', 3)

    :: (String, Int))
    where wrap i op (s, j) | i `op` j = s
          wrap _ _ (s, _) = "(" ++ s ++ ")"
