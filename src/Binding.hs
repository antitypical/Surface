module Binding where

import Name

data Binding expression term
  = Variable Name
  | Abstraction Name term
  | Expression expression
