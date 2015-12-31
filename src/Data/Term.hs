module Data.Term where

import Data.Binding
import Data.Foldable
import Data.Name
import qualified Data.Set as Set

data Term f = Term { freeVariables :: Set.Set Name, out :: Binding f (Term f) }

variable :: Name -> Term f
variable name = Term (Set.singleton name) (Variable name)

abstraction :: Name -> Term f -> Term f
abstraction name body = Term (Set.delete name $ freeVariables body) (Abstraction name body)

expression :: Foldable f => f (Term f) -> Term f
expression e = Term (foldMap freeVariables e) (Expression e)

maxBoundVariable :: (Foldable f, Functor f) => Term f -> Maybe Name
maxBoundVariable term = case out term of
  Variable _ -> Nothing
  Abstraction name _ -> Just name
  Expression e -> maximum (maxBoundVariable <$> e)

rename :: (Foldable f, Functor f) => Name -> Name -> Term f -> Term f
rename old new (Term _ binding) = case binding of
  Variable name -> if name == old then variable new else variable old
  Abstraction name body -> if name == old then abstraction name body else abstraction name (rename old new body)
  Expression body -> expression $ rename old new <$> body

substitute :: (Foldable f, Functor f) => Name -> Term f -> Term f -> Term f
substitute name with (Term _ binding) = case binding of
  Variable v -> if name == v then with else variable v
  Abstraction name body -> abstraction name' body'
    where name' = fresh (Set.union (freeVariables body) (freeVariables with)) name
          body' = substitute name with (rename name name' body)
  Expression body -> expression $ substitute name with <$> body

cata :: Functor f => (Binding f a -> a) -> Term f -> a
cata f = f . fmap (cata f) . out

para :: Functor f => (Binding f (Term f, a) -> a) -> Term f -> a
para f = f . fmap fanout . out
  where fanout a = (a, para f a)
