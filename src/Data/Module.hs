module Data.Module where

import Data.Expression
import Data.Term.Types
import qualified Data.Map as Map

data Module = Module (Map.Map String (Term Expression))

data Definition = Definition String (Term Expression)
