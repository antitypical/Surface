module Data.Term where

import Data.Binding
import Data.Typing
import Data.Foldable
import Data.Name
import qualified Data.Set as Set

data Term f = Term { freeVariables :: Set.Set Name, out :: Typing (Binding f) (Term f) }

variable :: Name -> Term f
variable name = Term (Set.singleton name) (Binding (Variable name))

abstraction :: Name -> Term f -> Term f
abstraction name body = Term (Set.delete name $ freeVariables body) (Binding (Abstraction name body))

typing :: Foldable f => Typing (Binding f) (Term f) -> Term f
typing t = Term (foldMap freeVariables t) t

binding :: Foldable f => Binding f (Term f) -> Term f
binding b = Term (foldMap freeVariables b) (Binding b)

expression :: Foldable f => f (Term f) -> Term f
expression e = Term (foldMap freeVariables e) (Binding (Expression e))

maxBoundVariable :: (Foldable f, Functor f) => Term f -> Maybe Name
maxBoundVariable = cata $ \ t -> case t of
  Annotation a b -> max a b
  Binding (Abstraction name _) -> Just name
  Binding (Expression e) -> maximum e
  _ -> Nothing

rename :: (Foldable f, Functor f) => Name -> Name -> Term f -> Term f
rename old new (Term _ binding) = case binding of
  Binding (Variable name) -> if name == old then variable new else variable old
  Binding (Abstraction name body) -> if name == old then abstraction name body else abstraction name (rename old new body)
  Binding (Expression body) -> expression $ rename old new <$> body

substitute :: (Foldable f, Functor f) => Name -> Term f -> Term f -> Term f
substitute name with (Term _ binding) = case binding of
  Binding (Variable v) -> if name == v then with else variable v
  Binding (Abstraction name body) -> abstraction name' body'
    where name' = fresh (Set.union (freeVariables body) (freeVariables with)) name
          body' = substitute name with (rename name name' body)
  Binding (Expression body) -> expression $ substitute name with <$> body

cata :: Functor f => (Typing (Binding f) a -> a) -> Term f -> a
cata f = f . fmap (cata f) . out

para :: Functor f => (Typing (Binding f) (Term f, a) -> a) -> Term f -> a
para f = f . fmap fanout . out
  where fanout a = (a, para f a)
