module Syntax.Pattern where

import Syntax (Lit)

data Pattern
  = PatVar String
  | PatCon String [Pattern]
  | PatLit Lit
  | PatWild
