module Data.Module where

import Data.Expression
import Data.Term.Types

data Module = Module [Definition]

data Definition = Definition { symbol :: String, getType :: Term Expression }
