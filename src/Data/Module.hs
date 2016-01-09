module Data.Module where

import Data.Expression
import qualified Data.List as List
import Data.Term.Types
import Surface.Language

data Module = Module [Definition Expression]

(!) :: Module -> String -> Definition Expression
Module definitions ! key = case List.find ((== key) . symbol) definitions of
  Just x -> x
  _ -> error "Expected definition to exist"

checkModule :: Context Term' -> Module -> Result [Term']
checkModule context (Module definitions) = sequence $ checkDefinition context <$> definitions

data Definition f = Definition { symbol :: String, getType :: Term f, getValue :: Term f }

checkDefinition :: Context Term' -> Definition Expression -> Result Term'
checkDefinition context (Definition symbol t v) = typeOf t _type' context >> typeOf v t context
