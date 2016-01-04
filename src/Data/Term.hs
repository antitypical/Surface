{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Data.Term where

import Data.Binding
import Data.Name
import Data.Typing
import Data.Term.Types
import Data.Unification
import qualified Data.Map as Map
import qualified Data.Set as Set

variable :: Name -> Term f
variable name = Term (Set.singleton name) (maybe (Left $ "Unexpectedly free variable " ++ show name) Right . Map.lookup name) (Binding (Variable name))

abstraction :: Name -> Term f -> Term f
abstraction name scope = Term (Set.delete name $ freeVariables scope) (typeOf scope) (Binding (Abstraction name scope))

abstract :: (Foldable f, Functor f) => (Term f -> Term f) -> Term f
abstract f = abstraction name scope
  where scope = f $ variable name
        name = maybe (Local 0) prime $ maxBoundVariable (f $ variable (Local $ negate 1))

-- | Construct the annotation of a term by a type. The term will be checked against this type.
annotation :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> Term f
annotation term type' = checkedTyping (check type' term) $ Annotation term type'


checkedAbstraction :: Name -> TypeChecker (Term f) -> Term f -> Term f
checkedAbstraction name typeChecker scope = Term (Set.delete name $ freeVariables scope) typeChecker (Binding (Abstraction name scope))

-- | Constructs an abstraction term with a name, the type of that name, and the scope which the name is available within.
typedAbstraction :: Name -> Term f -> Term f -> Term f
typedAbstraction name type' scope = checkedAbstraction name (typeOf scope . Map.insert name type') scope

checkedTyping :: Foldable f => TypeChecker (Term f) -> Typing (Binding f) (Term f) -> Term f
checkedTyping typeChecker t = Term (foldMap freeVariables t) typeChecker t

checkedBinding :: Foldable f => TypeChecker (Term f) -> Binding f (Term f) -> Term f
checkedBinding typeChecker = checkedTyping typeChecker . Binding

checkedExpression :: Foldable f => TypeChecker (Term f) -> f (Term f) -> Term f
checkedExpression typeChecker = checkedBinding typeChecker . Expression

_type :: Foldable f => Int -> Term f
_type n = Term mempty (const . Right . _type $ n + 1) $ Type n

_type' :: Foldable f => Term f
_type' = _type 0

implicit :: Term f
implicit = Term mempty (const $ Right implicit) Implicit

-- | Constructs a typechecker which verifies that the given type is inhabited by the given term.
check :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> TypeChecker (Term f)
check expected term context = do
  actual <- typeOf term context
  expectUnifiable expected actual

expectUnifiable :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> Result (Term f)
expectUnifiable expected actual = maybe (Left $ "error: Unification failed.\nExpected: '" ++ show expected ++ "'\n  Actual: '" ++ show actual ++ "'.\n") Right $ unified $ unify expected actual

maxBoundVariable :: (Foldable f, Functor f) => Term f -> Maybe Name
maxBoundVariable = cata $ \ t -> case t of
  Annotation a b -> max a b
  Binding (Abstraction name _) -> Just name
  Binding (Expression e) -> maximum e
  _ -> Nothing

rename :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Name -> Name -> Term f -> Term f
rename old new term | old == new = term
rename old new term@(Term _ typeChecker binding) = case binding of
  Binding (Variable name) -> if name == old then variable new else term
  Binding (Abstraction name scope) -> if name == old then term else checkedAbstraction name typeChecker (rename old new scope)
  Binding (Expression body) -> checkedExpression typeChecker $ rename old new <$> body
  Annotation a b -> let a' = rename old new a
                        b' = rename old new b in checkedTyping (check b' a') (Annotation a' b')
  Type _ -> term
  Implicit -> term

renameUnification :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Name -> Name -> Unification f -> Unification f
renameUnification old new (Unification out) = Unification $ renameTypingBy (renameUnification old new) old new out
renameUnification old new (Conflict expected actual) = Conflict (rename old new expected) (rename old new actual)

renameTypingBy :: Functor f => (g f -> g f) -> Name -> Name -> Typing (Binding f) (g f) -> Typing (Binding f) (g f)
renameTypingBy _ old new typing | old == new = typing
renameTypingBy f old new typing = case typing of
  Binding (Variable name) -> if name == old then Binding (Variable new) else typing
  Binding (Abstraction name scope) -> if name == old then typing else Binding $ Abstraction name (f scope)
  Binding (Expression body) -> Binding $ Expression $ f <$> body

  Annotation a b -> Annotation (f a) (f b)

  Type _ -> typing
  Implicit -> typing

substitute :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Name -> Term f -> Term f -> Term f
substitute name with term | with == variable name = term
substitute name with term@(Term _ typeChecker binding) = case binding of
  Binding (Variable v) -> if name == v then with else variable v
  Binding (Abstraction bound scope) -> if name == bound then term else abstraction bound' scope'
    where bound' = fresh (Set.union (freeVariables term) (freeVariables with)) bound
          scope' = substitute name with (rename bound bound' scope)
  Binding (Expression body) -> checkedExpression typeChecker $ substitute name with <$> body
  Annotation term' type' -> let term'' = substitute name with term'
                                type'' = substitute name with type' in annotation term'' type''
  Type _ -> term
  Implicit -> term

applySubstitution :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> Term f
applySubstitution withTerm body = case out body of
  Binding (Abstraction name inScope) -> substitute name withTerm inScope
  _ -> body

extendContext :: Term f -> Context (Term f) -> Term f -> Context (Term f)
extendContext type' context (Term _ _ binding) = case binding of
  Binding (Abstraction name _) -> Map.insert name type' context
  _ -> context

-- | Returns Just the name bound in term1 which shadows a variable free in term2, if any, or Nothing otherwise.
shadowing :: Term f -> Term f -> Maybe Name
shadowing term1 term2 = case out term1 of
  Binding (Abstraction name _) | Set.member name (freeVariables term2) -> Just name
  _ -> Nothing


cata :: Functor f => (Typing (Binding f) a -> a) -> Term f -> a
cata f = f . fmap (cata f) . out

para :: Functor f => (Typing (Binding f) (Term f, a) -> a) -> Term f -> a
para f = f . fmap fanout . out
  where fanout a = (a, para f a)

byUnifying :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => TypeChecker (Term f) -> TypeChecker (Term f) -> TypeChecker (Term f)
byUnifying a b context = do
  a' <- a context
  b' <- b context
  maybe (Left "couldn’t unify") Right $ unified $ unify a' b'

unify :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> Unification f
unify expected actual = case (out expected, out actual) of
  (a, b) | a == b -> into expected

  (_, Implicit) -> into expected
  (Implicit, _) -> into actual

  (Type _, Type _) -> into expected
  (Annotation term1 type1, Annotation term2 type2) -> Unification (Annotation (unify term1 term2) (unify type1 type2))

  (Binding (Abstraction name1 scope1), Binding (Abstraction name2 scope2)) | name1 == name2 -> Unification (Binding $ Abstraction name1 (unify scope1 scope2))
  (Binding (Abstraction name1 scope1), Binding (Abstraction name2 scope2)) -> Unification (Binding $ Abstraction name1 (renameUnification name name1 (unify (rename name1 name scope1) (rename name2 name scope2))))
    where name = pick $ freeVariables scope1 `mappend` freeVariables scope2

  (Binding (Abstraction name scope), _) | Set.notMember name (freeVariables scope) -> unify scope actual
  (_, Binding (Abstraction name scope)) | Set.notMember name (freeVariables scope) -> unify expected scope

  (Binding (Expression e1), Binding (Expression e2)) | Just e <- unifyBy unify e1 e2 -> Unification $ Binding $ Expression e

  _ -> Conflict expected actual
