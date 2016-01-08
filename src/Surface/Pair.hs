module Surface.Pair where

import Data.Expression
import Data.Module
import Data.Term.Types
import Surface.Language

_module :: Module
_module = Module $ [
    Definition "Pair" _Pair
  ]

_Pair :: Term Expression
_Pair = _type' `lambda` \ a -> _type' `lambda` \ b -> _type' `lambda` \ motive -> (a --> b --> motive) --> motive
