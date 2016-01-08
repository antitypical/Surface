module Data.Module where

import Data.Expression
import qualified Data.List as List
import Data.Term.Types

data Module = Module [Definition]

(!) :: Module -> String -> Definition
Module definitions ! key = case List.find ((== key) . symbol) definitions of
  Just x -> x
  _ -> error "Expected definition to exist"

data Definition = Definition { symbol :: String, getType :: Term Expression }
