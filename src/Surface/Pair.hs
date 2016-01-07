module Surface.Pair where

import Data.Expression
import Data.Term.Types
import Surface.Language

_Pair :: Term Expression
_Pair = _type' `lambda` \ a -> _type' `lambda` \ b -> _type' `lambda` \ motive -> (a --> b --> motive) --> motive
