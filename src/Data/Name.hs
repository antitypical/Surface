module Data.Name where

import Data.Set

data Name = Local Int | Global String
  deriving (Show, Eq, Ord)

freshBy :: (Name -> Bool) -> Name -> Name
freshBy used name | used name = case name of
  Local i -> freshBy used (Local $ i + 1)
  Global n -> freshBy used (Global $ n ++ "'")
freshBy _ name = name

fresh :: Set Name -> Name -> Name
fresh set = freshBy (`member` set)
