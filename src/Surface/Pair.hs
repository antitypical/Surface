module Surface.Pair where

import Data.Module
import Surface.Language

_module :: Module Term'
_module = Module [
    Definition "Pair"
      (_type' --> _type' --> _type')
      (_type' `lambda` \ a -> _type' `lambda` \ b -> _type' `lambda` \ motive -> (a --> b --> motive) --> motive)
  ]
