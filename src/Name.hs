module Name where

data Name = Local Int | Global String
  deriving (Show, Eq, Ord)

freshBy :: (Name -> Bool) -> Name -> Name
freshBy used name | used name = case name of
  Local i -> freshBy used (Local $ i + 1)
  Global n -> freshBy used (Global $ n ++ "'")
freshBy _ name = name
