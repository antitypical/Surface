module Data.Module where

import Data.Expression
import Data.Term
import qualified Data.Map as Map

data Module = Module (Map.Map String (Term Expression))
