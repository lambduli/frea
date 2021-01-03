module Compiler.Syntax.Pattern where

import Compiler.Syntax (Lit)

data Pattern
  = PatVar String
  | PatCon String [Pattern]
  | PatLit Lit
  | PatWild
  deriving (Show)
