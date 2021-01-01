module Compiler.Syntax.Literal where

data Lit
  = LitInt Int
  | LitDouble Double
  | LitChar Char
  | LitString String