{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Data.Term where

import Data.Binding
import Data.Name
import Data.Typing
import Data.Term.Types
import Data.Unification
import qualified Data.Map as Map
import qualified Data.Set as Set

variable :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Name -> Term f
variable name = Term (Set.singleton name) (checkInferred (maybe (Left $ "Unexpectedly free variable " ++ show name) Right . Map.lookup name)) (Binding (Variable name))

abstraction :: Name -> Term f -> Term f
abstraction name scope = Term (Set.delete name $ freeVariables scope) (typeOf scope) (Binding (Abstraction name scope))

abstract :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => (Term f -> Term f) -> Term f
abstract f = abstraction name scope
  where scope = f $ variable name
        name = maybe (Local 0) prime $ maxBoundVariable (f $ variable (Local $ negate 1))

-- | Construct the annotation of a term by a type. The term will be checked against this type.
annotation :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> Term f
annotation term type' = checkedTyping typeChecker $ Annotation term type'
  where typeChecker against context = typeOf term type' context >>= expectUnifiable against


checkedAbstraction :: Name -> Checker (Term f) -> Term f -> Term f
checkedAbstraction name typeChecker scope = Term (Set.delete name $ freeVariables scope) typeChecker (Binding (Abstraction name scope))

-- | Constructs an abstraction term with a name, the type of that name, and the scope which the name is available within.
typedAbstraction :: Name -> Term f -> Term f -> Term f
typedAbstraction name type' scope = checkedAbstraction name typeChecker scope
  where typeChecker against = typeOf scope against . Map.insert name type'


checkedTyping :: Foldable f => Checker (Term f) -> Typing (Binding f) (Term f) -> Term f
checkedTyping typeChecker t = Term (foldMap freeVariables t) typeChecker t

checkedBinding :: Foldable f => Checker (Term f) -> Binding f (Term f) -> Term f
checkedBinding typeChecker = checkedTyping typeChecker . Binding

checkedExpression :: Foldable f => Checker (Term f) -> f (Term f) -> Term f
checkedExpression typeChecker = checkedBinding typeChecker . Expression

implicit :: Term f
implicit = Term mempty (const . Right) Implicit

expectUnifiable :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Term f -> Term f -> Result (Term f)
expectUnifiable expected actual = maybe (Left $ "error: Unification failed.\nExpected: '" ++ show expected ++ "'\n  Actual: '" ++ show actual ++ "'.\n") Right $ unified $ unify expected actual

checkInferred :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Inferer (Term f) -> Checker (Term f)
checkInferred inferer expected context = do
  actual <- inferer context
  expected `expectUnifiable` actual

inferTypeOf :: Term f -> Inferer (Term f)
inferTypeOf = (`typeOf` implicit)

maxBoundVariable :: (Foldable f, Functor f) => Term f -> Maybe Name
maxBoundVariable = cata $ \ t -> case t of
  Annotation a b -> max a b
  Binding (Abstraction name _) -> Just name
  Binding (Expression e) -> maximum e
  _ -> Nothing

rename :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Name -> Name -> Term f -> Term f
rename old new (Term freeVariables typeChecker typing) = Term (replace old new freeVariables) typeChecker $ renameTypingBy (rename old new) old new typing

replace :: Ord a => a -> a -> Set.Set a -> Set.Set a
replace old new set = if Set.member old set then Set.insert new $ Set.delete old set else set

renameUnification :: (Show (Term f), Unifiable f, Traversable f, Eq (f (Term f))) => Name -> Name -> Unification f -> Unification f
renameUnification old new (Unification freeVariables typeChecker out) = Unification (replace old new freeVariables) typeChecker $ renameTypingBy (renameUnification old new) old new out
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
