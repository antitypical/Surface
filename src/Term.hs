module Term where

import Binding
import Name
import Data.Foldable

data Term f = Term { freeVariables :: [Name], out :: Binding f (Term f) }

variable :: Name -> Term f
variable name = Term [ name ] (Variable name)

abstraction :: Name -> Term f -> Term f
abstraction name body = Term (filter (/= name) $ freeVariables body) (Abstraction name body)

expression :: (Foldable f, Functor f) => f (Term f) -> Term f
expression e = Term (foldMap freeVariables e) (Expression e)
