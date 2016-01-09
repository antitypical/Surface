module Data.Module where

import qualified Data.List as List
import Data.Term.Types
import Surface.Language

data Module = Module [Definition Term']

(!) :: Module -> String -> Definition Term'
Module definitions ! key = case List.find ((== key) . symbol) definitions of
  Just x -> x
  _ -> error "Expected definition to exist"

checkModule :: Context Term' -> Module -> Result [Term']
checkModule context (Module definitions) = sequence $ checkDefinition context <$> definitions

data Definition term = Definition { symbol :: String, getType :: term, getValue :: term }

checkDefinition :: Context Term' -> Definition Term' -> Result Term'
checkDefinition context (Definition symbol t v) = typeOf t _type' context >> typeOf v t context
