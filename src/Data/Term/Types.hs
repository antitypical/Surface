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

data Unification f = Unification (Term' (Unification f) f) | Replace (Term f) (Term f)
