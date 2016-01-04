{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Data.Unification where

import Data.Binding
import Data.Typing
import Data.Term.Types
import qualified Data.Set as Set

class Unifiable e where
  unifyBy :: (f e -> f e -> Unification e) -> e (f e) -> e (f e) -> Maybe (e (Unification e))


data Unification f = Unification (Typing (Binding f) (Unification f)) | Conflict (Term f) (Term f)

deriving instance (Eq (Term f), Eq (f (Unification f))) => Eq (Unification f)
deriving instance (Show (Term f), Show (f (Unification f))) => Show (Unification f)

expected :: Functor f => Unification f -> Term f
expected (Conflict expected _) = expected
expected (Unification out) = Term Set.empty (const $ Left "Unification does not preserve typecheckers") (expected <$> out)

actual :: Functor f => Unification f -> Term f
actual (Conflict _ actual) = actual
actual (Unification out) = Term Set.empty (const $ Left "Unification does not preserve typecheckers") (actual <$> out)

unified :: Traversable f => Unification f -> Maybe (Term f)
unified (Conflict _ _) = Nothing
unified (Unification out) = do
  out <- mapM unified out
  return $ Term Set.empty (const $ Left "Unification does not preserve typecheckers") out

into :: Functor f => Term f -> Unification f
into term = Unification $ into <$> out term

instance Renameable (Unification f) where
  rename' old new (Unification out) = Unification out
  rename' _ _ u = u
