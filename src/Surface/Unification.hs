module Surface.Unification where

import Data.Expression
import Data.Term.Types

class Unifiable e where
  unifyBy :: (f e -> f e -> Unification e) -> e (f e) -> e (f e) -> Maybe (e (Unification e))

instance Unifiable Expression where
  unifyBy f expected actual = case (expected, actual) of
    (Application a1 b1, Application a2 b2) -> Just $ Application (f a1 a2) (f b1 b2)
    (Lambda a1 b1, Lambda a2 b2) -> Just $ Lambda (f a1 a2) (f b1 b2)
    _ -> Nothing
