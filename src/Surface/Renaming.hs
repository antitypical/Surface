{-# LANGUAGE FlexibleContexts #-}
module Surface.Renaming where

import Data.Binding
import Data.Name
import Data.Term.Types
import Data.Typing
import qualified Data.Set as Set

rename :: Functor f => Name -> Name -> Term f -> Term f
rename old new (Term freeVariables typeChecker typing) = Term (replace old new freeVariables) typeChecker $ renameTypingBy (rename old new) old new typing

replace :: Ord a => a -> a -> Set.Set a -> Set.Set a
replace old new set = if Set.member old set then Set.insert new $ Set.delete old set else set

renameUnification :: Functor f => Name -> Name -> Unification f -> Unification f
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
