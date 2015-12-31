{-# LANGUAGE DeriveFunctor #-}
module Term where

import Binding
import Name

data Term f = Term { freeVariables :: [Name], expression :: Binding f (Term f) }
  deriving (Functor, Show, Eq)

variable :: Name -> Term f
variable name = Term [ name ] (Variable name)

abstraction :: Name -> Term f -> Term f
abstraction name body = Term (filter (/= name) $ freeVariables body) (Abstraction name body)
