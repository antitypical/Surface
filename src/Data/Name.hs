module Data.Name (
  Name(..),
  freshBy,
  fresh,
  pick,
  prime,
) where

import Data.Name.Internal
import Data.Set

data Name = Local Int | Global String
  deriving (Eq, Ord)

instance Show Name where
  show (Local i) = showNumeral "abcdefghijklmnopqrstuvwxyz" i
  show (Global name) = name

freshBy :: (Name -> Bool) -> Name -> Name
freshBy used name | used name = freshBy used (prime name)
freshBy _ name = name

fresh :: Set Name -> Name -> Name
fresh set = freshBy (`member` set)

pick :: Set Name -> Name
pick set = fresh set (maximum set)

prime :: Name -> Name
prime (Local i) = Local $ i + 1
prime (Global n) = Global $ n ++ "ʹ"
