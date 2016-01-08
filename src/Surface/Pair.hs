module Surface.Pair where

import Data.Module
import Surface.Language

_module :: Module
_module = Module [
    Definition "Pair"
      (_type' `lambda` \ a -> _type' `lambda` \ b -> _type' `lambda` \ motive -> (a --> b --> motive) --> motive)
      (_type' `lambda` \ a -> _type' `lambda` \ b -> _type' `lambda` \ motive -> (a --> b --> motive) --> motive)
  ]
