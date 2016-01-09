{-# LANGUAGE FlexibleContexts #-}
module Data.Module where

import qualified Data.List as List
import Data.Term.Types
import Data.Unification
import Surface.Language

data Module f = Module [Definition f]

(!) :: Module f -> String -> Definition f
Module definitions ! key = case List.find ((== key) . symbol) definitions of
  Just x -> x
  _ -> error "Expected definition to exist"

checkModule :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Context (Term f) -> Module f -> Result [Term f]
checkModule context (Module definitions) = sequence $ checkDefinition context <$> definitions

data Definition f = Definition { symbol :: String, getType :: Term f, getValue :: Term f }

checkDefinition :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Context (Term f) -> Definition f -> Result (Term f)
checkDefinition context (Definition symbol t v) = typeOf t _type' context >> typeOf v t context
