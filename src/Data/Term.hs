{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Data.Term where

import Data.Binding
import Data.Name
import Data.Typing
import Data.Unification
import qualified Data.Map as Map
import qualified Data.Set as Set

type Result a = Either String a

type Context f = Map.Map Name (Term f)

type TypeChecker f = Context f -> Result (Term f)

data Term f = Term { freeVariables :: Set.Set Name, typeOf :: TypeChecker f, out :: Typing (Binding f) (Term f) }

variable :: Name -> Term f
variable name = Term (Set.singleton name) (maybe (Left $ "Unexpectedly free variable " ++ show name) Right . Map.lookup name) (Binding (Variable name))

abstraction :: Name -> Term f -> Term f
abstraction name scope = Term (Set.delete name $ freeVariables scope) (typeOf scope) (Binding (Abstraction name scope))

abstract :: (Foldable f, Functor f) => (Term f -> Term f) -> Term f
abstract f = abstraction name scope
  where scope = f $ variable name
        name = maybe (Local 0) prime $ maxBoundVariable (f $ variable (Local $ negate 1))

-- | Construct the annotation of a term by a type. The term will be checked against this type.
annotation :: (Functor f, Foldable f, Show (Term f), Unifiable (f (Term f)), Eq (f (Term f))) => Term f -> Term f -> Term f
annotation term type' = checkedTyping (check type' term) $ Annotation term type'


checkedAbstraction :: Name -> TypeChecker f -> Term f -> Term f
checkedAbstraction name typeChecker scope = Term (Set.delete name $ freeVariables scope) typeChecker (Binding (Abstraction name scope))

-- | Constructs an abstraction term with a name, the type of that name, and the scope which the name is available within.
typedAbstraction :: Name -> Term f -> Term f -> Term f
typedAbstraction name type' scope = checkedAbstraction name (typeOf scope . Map.insert name type') scope

checkedTyping :: Foldable f => TypeChecker f -> Typing (Binding f) (Term f) -> Term f
checkedTyping typeChecker t = Term (foldMap freeVariables t) typeChecker t

checkedBinding :: Foldable f => TypeChecker f -> Binding f (Term f) -> Term f
checkedBinding typeChecker = checkedTyping typeChecker . Binding

checkedExpression :: Foldable f => TypeChecker f -> f (Term f) -> Term f
checkedExpression typeChecker = checkedBinding typeChecker . Expression

_type :: Foldable f => Int -> Term f
_type n = Term mempty (const . Right . _type $ n + 1) $ Type n

_type' :: Foldable f => Term f
_type' = _type 0

implicit :: Term f
implicit = Term mempty (const $ Right implicit) Implicit

-- | Constructs a typechecker which verifies that the given type is inhabited by the given term.
check :: (Show (Term f), Unifiable (Term f)) => Term f -> Term f -> TypeChecker f
check expected term context = do
  actual <- typeOf term context
  maybe (Left $ "error: Unification failed.\nExpected: '" ++ show expected ++ "'\n  Actual: '" ++ show actual ++ "'") Right $ unify expected actual

checkIsType :: (Show (Term f), Unifiable (Term f), Foldable f) => Term f -> TypeChecker f
checkIsType = check _type'

maxBoundVariable :: (Foldable f, Functor f) => Term f -> Maybe Name
maxBoundVariable = cata $ \ t -> case t of
  Annotation a b -> max a b
  Binding (Abstraction name _) -> Just name
  Binding (Expression e) -> maximum e
  _ -> Nothing

rename :: (Foldable f, Functor f, Show (Term f), Unifiable (f (Term f)), Eq (f (Term f))) => Name -> Name -> Term f -> Term f
rename old new term | old == new = term
rename old new term@(Term _ typeChecker binding) = case binding of
  Binding (Variable name) -> if name == old then variable new else term
  Binding (Abstraction name scope) -> if name == old then term else checkedAbstraction name typeChecker (rename old new scope)
  Binding (Expression body) -> checkedExpression typeChecker $ rename old new <$> body
  Annotation a b -> let a' = rename old new a
                        b' = rename old new b in checkedTyping (check b' a') (Annotation a' b')
  Type _ -> term
  Implicit -> term

substitute :: (Foldable f, Functor f, Show (Term f), Unifiable (f (Term f)), Eq (f (Term f))) => Name -> Term f -> Term f -> Term f
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

applySubstitution :: (Foldable f, Functor f, Show (Term f), Unifiable (f (Term f)), Eq (f (Term f))) => Term f -> Term f -> Term f
applySubstitution withTerm body = case out body of
  Binding (Abstraction name inScope) -> substitute name withTerm inScope
  _ -> body

extendContext :: Term f -> Context f -> Term f -> Context f
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

byUnifying :: (Unifiable (Term f)) => TypeChecker f -> TypeChecker f -> TypeChecker f
byUnifying a b context = do
  a' <- a context
  b' <- b context
  maybe (Left "couldn’t unify") Right $ unify a' b'

instance Eq (f (Term f)) => Eq (Term f) where
  a == b = freeVariables a == freeVariables b && out a == out b

instance (Functor f, Foldable f, Show (Term f), Eq (f (Term f)), Unifiable (f (Term f))) => Unifiable (Term f) where
  unify expected actual = case (out expected, out actual) of
    (a, b) | a == b -> Just expected

    (_, Implicit) -> Just expected
    (Implicit, _) -> Just actual

    (Type _, Type _) -> Just expected
    (Annotation term1 type1, Annotation term2 type2) -> do
      term <- unify term1 term2
      type' <- unify type1 type2
      return $ checkedTyping (byUnifying (typeOf expected) (typeOf actual)) (Annotation term type')

    (Binding (Abstraction name1 scope1), Binding (Abstraction name2 scope2)) -> do
        let name = if name1 == name2
            then name1
            else pick $ freeVariables scope1 `mappend` freeVariables scope2
        scope <- unify (rename name1 name scope1) (rename name2 name scope2)
        return $ checkedAbstraction name (byUnifying (typeOf expected) (typeOf actual)) (rename name name1 scope)

    (Binding (Abstraction name scope), _) | Set.notMember name (freeVariables scope) -> unify scope actual
    (_, Binding (Abstraction name scope)) | Set.notMember name (freeVariables scope) -> unify expected scope

    (Binding (Expression e1), Binding (Expression e2)) -> do
      e <- unify e1 e2
      return $ checkedExpression (byUnifying (typeOf expected) (typeOf actual)) e

    _ -> Nothing
