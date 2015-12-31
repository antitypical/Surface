module Data.Term where

import Data.Binding
import Data.Foldable
import Data.Name
import Data.Set

data Term f = Term { freeVariables :: Set Name, out :: Binding f (Term f) }

variable :: Name -> Term f
variable name = Term (singleton name) (Variable name)

abstraction :: Name -> Term f -> Term f
abstraction name body = Term (delete name $ freeVariables body) (Abstraction name body)

expression :: Foldable f => f (Term f) -> Term f
expression e = Term (foldMap freeVariables e) (Expression e)

maxBoundVariable :: (Foldable f, Functor f) => Term f -> Maybe Name
maxBoundVariable term = case out term of
  Variable _ -> Nothing
  Abstraction name _ -> Just name
  Expression e -> maximum (maxBoundVariable <$> e)