module Data.Term.Types where

import Data.Binding
import Data.Name
import Data.Typing
import qualified Data.Map as Map
import qualified Data.Set as Set

type Result a = Either String a

type Context term = Map.Map Name term

type TypeChecker term = Context term -> Result term

data Term' term f = Term' (Set.Set Name) (TypeChecker term) (Typing (Binding f) term)
newtype Term f = Term (Term' (Term f) f)

freeVariables :: Term f -> Set.Set Name
freeVariables (Term (Term' freeVariables _ _)) = freeVariables

typeOf :: Term f -> TypeChecker (Term f)
typeOf (Term (Term' _ typeOf _)) = typeOf

out :: Term f -> Typing (Binding f) (Term f)
out (Term (Term' _ _ out)) = out


data Unification f = Unification (Term' (Unification f) f) | Replace (Term f) (Term f)
