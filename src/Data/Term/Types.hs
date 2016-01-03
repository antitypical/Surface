module Data.Term.Types where

import Data.Binding
import Data.Name
import Data.Typing
import qualified Data.Map as Map
import qualified Data.Set as Set

type Result a = Either String a

type Context term = Map.Map Name term

type TypeChecker term = Context term -> Result term

data Term f = Term { freeVariables :: Set.Set Name, typeOf :: TypeChecker (Term f), out :: Typing (Binding f) (Term f) }

data Unification f = Unification (Typing (Binding f) (Unification f)) | Replace (Term f) (Term f)

expected :: Functor f => Unification f -> Term f
expected (Replace expected _) = expected
expected (Unification out) = Term Set.empty (const $ Left "Unification does not preserve typecheckers") (expected <$> out)

actual :: Functor f => Unification f -> Term f
actual (Replace _ actual) = actual
actual (Unification out) = Term Set.empty (const $ Left "Unification does not preserve typecheckers") (actual <$> out)

unified :: Traversable f => Unification f -> Maybe (Term f)
unified (Replace _ _) = Nothing
unified (Unification out) = do
  out <- mapM unified out
  return $ Term Set.empty (const $ Left "Unification does not preserve typecheckers") out
