module Term where

import Binding
import Name

data Term f = Term { freeVariables :: [Name], out :: Binding f (Term f) }
  deriving (Show, Eq)

variable :: Name -> Term f
variable name = Term [ name ] (Variable name)

abstraction :: Name -> Term f -> Term f
abstraction name body = Term (filter (/= name) $ freeVariables body) (Abstraction name body)
