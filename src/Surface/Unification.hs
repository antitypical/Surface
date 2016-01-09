{-# LANGUAGE FlexibleContexts #-}
module Surface.Unification where

import Data.Binding
import Data.Expression
import Data.Name
import Data.Term.Types
import Data.Typing
import qualified Data.Set as Set
import Surface.Renaming

instance Unifiable Expression where
  unifyBy f expected actual = case (expected, actual) of
    (Application a1 b1, Application a2 b2) -> Just $ Application (f a1 a2) (f b1 b2)
    (Lambda a1 b1, Lambda a2 b2) -> Just $ Lambda (f a1 a2) (f b1 b2)
    _ -> Nothing

byUnifying :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Checker (Term f) -> Checker (Term f) -> Checker (Term f)
byUnifying a b against context = do
  a' <- a against context
  b' <- b against context
  expectUnifiable a' b'

unify :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> Unification f
unify expected actual = case (out expected, out actual) of
  (a, b) | a == b -> into expected

  (_, Implicit) -> into expected
  (Implicit, _) -> into actual

  (Type _, Type _) -> into expected
  (Annotation term1 type1, Annotation term2 type2) -> Unification free (byUnifying (typeOf expected) (typeOf actual)) (Annotation (unify term1 term2) (unify type1 type2))

  (Binding (Abstraction name1 scope1), Binding (Abstraction name2 scope2)) | name1 == name2 -> Unification free (byUnifying (typeOf expected) (typeOf actual)) (Binding $ Abstraction name1 (unify scope1 scope2))
  (Binding (Abstraction name1 scope1), Binding (Abstraction name2 scope2)) -> Unification (Set.delete name1 free) (byUnifying (typeOf expected) (typeOf actual)) (Binding $ Abstraction name1 (renameUnification name name1 (unify (rename name1 name scope1) (rename name2 name scope2))))
    where name = pick free
          free = freeVariables scope1 `Set.union` freeVariables scope2

  (Binding (Abstraction name scope), _) | Set.notMember name (freeVariables scope) -> unify scope actual
  (_, Binding (Abstraction name scope)) | Set.notMember name (freeVariables scope) -> unify expected scope

  (Binding (Expression e1), Binding (Expression e2)) | Just e <- unifyBy unify e1 e2 -> Unification free (byUnifying (typeOf expected) (typeOf actual)) $ Binding $ Expression e

  _ -> Conflict expected actual
  where free = freeVariables expected `Set.union` freeVariables actual

expectUnifiable :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> Result (Term f)
expectUnifiable expected actual = maybe (Left $ "error: Unification failed.\nExpected: '" ++ show expected ++ "'\n  Actual: '" ++ show actual ++ "'.\n") Right $ unified $ unify expected actual
