module Data.Name (
  Name(..),
  freshBy,
  fresh,
) where

import Data.Name.Internal
import Data.Set

data Name = Local Int | Global String
  deriving (Eq, Ord)

instance Show Name where
  show (Local i) = showNumeral "abcdefghijklmnopqrstuvwxyz" i
  show (Global name) = name

freshBy :: (Name -> Bool) -> Name -> Name
freshBy used name | used name = case name of
  Local i -> freshBy used (Local $ i + 1)
  Global n -> freshBy used (Global $ n ++ "'")
freshBy _ name = name

fresh :: Set Name -> Name -> Name
fresh set = freshBy (`member` set)
