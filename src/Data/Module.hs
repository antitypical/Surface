module Data.Module where

import qualified Data.List as List
import Data.Term.Types
import Surface.Language

data Module = Module [Definition]

(!) :: Module -> String -> Definition
Module definitions ! key = case List.find ((== key) . symbol) definitions of
  Just x -> x
  _ -> error "Expected definition to exist"

checkModule :: Context Term' -> Module -> Result [Term']
checkModule context (Module definitions) = sequence $ checkDefinition context <$> definitions

data Definition = Definition { symbol :: String, getType :: Term', getValue :: Term' }

checkDefinition :: Context Term' -> Definition -> Result Term'
checkDefinition context (Definition symbol t v) = typeOf t _type' context >> typeOf v t context
