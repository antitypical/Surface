{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Data.Term.Types where

import Data.Binding
import Data.Name
import Data.Typing
import qualified Data.Map as Map
import qualified Data.Set as Set

type Result a = Either String a

type Context term = Map.Map Name term

type Inferer term = Context term -> Result term
type Checker term = term -> Context term -> Result term

data Term f = Term { freeVariables :: Set.Set Name, typeOf :: Checker (Term f), out :: Typing (Binding f) (Term f) }

data Unification f = Unification (Set.Set Name) (Checker (Term f)) (Typing (Binding f) (Unification f)) | Conflict (Term f) (Term f)

expected :: Functor f => Unification f -> Term f
expected (Conflict expected _) = expected
expected (Unification freeVariables typeChecker out) = Term freeVariables typeChecker (expected <$> out)

actual :: Functor f => Unification f -> Term f
actual (Conflict _ actual) = actual
actual (Unification freeVariables typeChecker out) = Term freeVariables typeChecker (actual <$> out)

unified :: Traversable f => Unification f -> Maybe (Term f)
unified (Conflict _ _) = Nothing
unified (Unification freeVariables typeChecker out) = do
  out <- mapM unified out
  return $ Term freeVariables typeChecker out

into :: Functor f => Term f -> Unification f
into term = Unification (freeVariables term) (typeOf term) $ into <$> out term


instance Eq (f (Term f)) => Eq (Term f) where
  a == b = freeVariables a == freeVariables b && out a == out b

instance (Eq (Term f), Eq (f (Unification f))) => Eq (Unification f) where
  (Unification freeVariablesA _ outA) == (Unification freeVariablesB _ outB) = freeVariablesA == freeVariablesB && outA == outB

instance (Functor f, Show (Term f), Show (f (Unification f))) => Show (Unification f) where
  show u = "Expected: " ++ show (expected u) ++ "\n"
        ++ "  Actual: " ++ show (actual u) ++ "\n"
