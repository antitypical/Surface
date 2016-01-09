module Surface.Unification where

import Data.Term.Types

class Unifiable e where
  unifyBy :: (f e -> f e -> Unification e) -> e (f e) -> e (f e) -> Maybe (e (Unification e))
