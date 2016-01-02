{-# LANGUAGE UndecidableInstances #-}
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
abstraction name scope = Term (Set.delete name $ freeVariables scope) (const $ Right implicit) (Binding (Abstraction name scope))

-- | Constructs an abstraction term with a name, the type of that name, and the scope which the name is available within.
checkedAbstraction :: Name -> Term f -> Term f -> Term f
checkedAbstraction name type' scope = Term (Set.delete name $ freeVariables scope) (typeOf scope . Map.insert name type') (Binding (Abstraction name scope))

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
check :: (Unifiable (f (Term f))) => Term f -> Term f -> TypeChecker f
check expected term context = do
  actual <- typeOf term context
  maybe (Left "couldn't unify") Right $ unify expected actual

checkIsType :: (Unifiable (f (Term f)), Foldable f) => Term f -> TypeChecker f
checkIsType = check _type'

maxBoundVariable :: (Foldable f, Functor f) => Term f -> Maybe Name
maxBoundVariable = cata $ \ t -> case t of
  Annotation a b -> max a b
  Binding (Abstraction name _) -> Just name
  Binding (Expression e) -> maximum e
  _ -> Nothing

rename :: (Foldable f, Functor f, Unifiable (f (Term f))) => Name -> Name -> Term f -> Term f
rename old new term@(Term _ type' binding) = case binding of
  Binding (Variable name) -> if name == old then variable new else variable old
  Binding (Abstraction name body) -> if name == old then abstraction name body else abstraction name (rename old new body)
  Binding (Expression body) -> checkedExpression type' $ rename old new <$> body
  Annotation a b -> let a' = rename old new a
                        b' = rename old new b in checkedTyping (check b' a') (Annotation a' b')
  Type _ -> term
  Implicit -> term

substitute :: (Foldable f, Functor f, Unifiable (f (Term f))) => Name -> Term f -> Term f -> Term f
substitute name with term@(Term _ type' binding) = case binding of
  Binding (Variable v) -> if name == v then with else variable v
  Binding (Abstraction name scope) -> abstraction name' scope'
    where name' = fresh (Set.union (freeVariables scope) (freeVariables with)) name
          scope' = substitute name with (rename name name' scope)
  Binding (Expression body) -> checkedExpression type' $ substitute name with <$> body
  Annotation a b -> let a' = substitute name with a
                        b' = substitute name with b in checkedTyping (check b' a') (Annotation a' b')
  Type _ -> term
  Implicit -> term

applySubstitution :: (Foldable f, Functor f, Unifiable (f (Term f))) => Term f -> Term f -> Term f
applySubstitution withTerm body = case out body of
  Binding (Abstraction name inScope) -> substitute name withTerm inScope
  _ -> body

extendContext :: Term f -> Context f -> Term f -> Context f
extendContext type' context (Term _ _ binding) = case binding of
  Binding (Abstraction name _) -> Map.insert name type' context
  _ -> context

cata :: Functor f => (Typing (Binding f) a -> a) -> Term f -> a
cata f = f . fmap (cata f) . out

para :: Functor f => (Typing (Binding f) (Term f, a) -> a) -> Term f -> a
para f = f . fmap fanout . out
  where fanout a = (a, para f a)


instance Unifiable (f (Term f)) => Unifiable (Term f) where
  unify a b = case (out a, out b) of
    (_, Implicit) -> Just a
    (Implicit, _) -> Just b
    _ -> Nothing
