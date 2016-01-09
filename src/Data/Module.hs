{-# LANGUAGE FlexibleContexts #-}
module Data.Module where

import qualified Data.List as List
import Data.Term.Types
import Data.Unification
import Surface.Language

data Module term = Module [Definition term]

(!) :: Module term -> String -> Definition term
Module definitions ! key = case List.find ((== key) . symbol) definitions of
  Just x -> x
  _ -> error "Expected definition to exist"

checkModule :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Context (Term f) -> Module (Term f) -> Result [Term f]
checkModule context (Module definitions) = sequence $ checkDefinition context <$> definitions

data Definition term = Definition { symbol :: String, getType :: term, getValue :: term }

checkDefinition :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Context (Term f) -> Definition (Term f) -> Result (Term f)
checkDefinition context (Definition symbol t v) = typeOf t _type' context >> typeOf v t context
