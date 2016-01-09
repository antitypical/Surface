{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Surface.Language where

import Control.Applicative
import Data.Binding
import Data.Expression
import Data.Name.Internal
import Data.Term
import Data.Term.Types
import Data.Typing
import Data.Unification
import Prelude hiding (pi)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Term' = Term Expression

_type :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Int -> Term f
_type n = Term mempty (checkInferred $ const $ Right (_type (n + 1))) $ Type n

_type' :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f
_type' = _type 0


infixr `lambda`

-- | Construct a lambda from a type and a function from an argument variable to the resulting term. The variable will be picked automatically. The parameter type will be checked against `Type`, but there are no constraints on the type of the result.
lambda :: Term Expression -> (Term Expression -> Term Expression) -> Term Expression
lambda t f = checkedExpression (checkFunctionType checkTypeAgainst) $ Lambda t body
  where body = abstract f
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
pi t f = checkedExpression (checkFunctionType checkTypeAgainst) $ Lambda t body
  where body = abstract f
        checkTypeAgainst from to context = do
          _ <- checkIsType t context
          _ <- typeOf t from context
          bodyType <- checkIsType body (extendContext t context body)
          _ <- typeOf bodyType to context
          return $ case shadowing body bodyType of
            Just name -> t `pi` \ v -> substitute name v bodyType
            _ -> t --> bodyType


infixr -->

-- | Construct a non-dependent function type between two types. Both operands will be checked against `Type`.
(-->) :: Term Expression -> Term Expression -> Term Expression
a --> b = checkedExpression (checkFunctionType checkTypeAgainst) $ Lambda a b
  where checkTypeAgainst from to context = do
          a' <- checkIsType a context
          _ <- typeOf a from context
          b' <- checkIsType b context
          _ <- typeOf b to context
          return $ a' --> b'

-- | Construct the application of one term to another. The first argument will be checked as a function type, and the second will be checked against that type’s domain. The resulting type will be the substitution of the domain type into the body.
apply :: Term Expression -> Term Expression -> Term Expression
apply a b = checkedExpression (checkInferred inferType) $ Application a b
  where inferType context = do
          (type', body) <- checkHasFunctionType a context
          _ <- typeOf b type' context
          return $ applySubstitution type' body


checkIsType :: Term Expression -> Inferer (Term Expression)
checkIsType term = typeOf term _type'


checkHasFunctionType :: Term Expression -> Context (Term Expression) -> Result (Term Expression, Term Expression)
checkHasFunctionType term context = inferTypeOf term context >>= checkIsFunctionType

checkIsFunctionType :: Term Expression -> Result (Term Expression, Term Expression)
checkIsFunctionType type' = case out type' of
  Binding (Expression (Lambda type' body)) -> return (type', body)
  _ -> Left "Expected function type."


checkFunctionType :: (Term Expression -> Term Expression -> Context (Term Expression) -> Result (Term Expression)) -> Checker (Term Expression)
checkFunctionType check expected context = case out expected of
  Binding (Abstraction _ scope) -> checkFunctionType check scope context
  Binding (Expression (Lambda from to)) -> check from to context
  Type _ -> check _type' _type' context
  _ -> check implicit implicit context >>= expectUnifiable expected


weakHeadNormalForm :: Environment (Term Expression) -> Term Expression -> Term Expression
weakHeadNormalForm environment term = case out term of
  Binding (Variable name) -> case Map.lookup name environment of
    Just v -> v
    _ -> term

  Binding (Expression (Application a b)) -> case out $ weakHeadNormalForm environment a of
    (Binding (Expression (Lambda _ body))) -> weakHeadNormalForm environment $ applySubstitution b body
    _ -> term

  _ -> term

instance Monoid a => Alternative (Either a) where
  empty = Left mempty
  Left a <|> Left b = Left $ a `mappend` b
  Right a <|> _ = Right a
  _ <|> Right b = Right b


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
