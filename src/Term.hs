module Term where

import Binding
import Name

data Term f = Term { freeVariables :: [Name], expression :: Binding f (Term f) }
