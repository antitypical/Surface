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
