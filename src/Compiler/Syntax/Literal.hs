module Compiler.Syntax.Literal where


data Lit
  = LitInt Int
  | LitBool Bool
  | LitDouble Double
  | LitChar Char
  | LitString String
  | LitUnit
  deriving (Eq)

instance Show Lit where
  show (LitInt i) = show i
  show (LitBool False) = "#f"
  show (LitBool True) = "#t"
  show (LitDouble d) = show d
  show (LitChar ch) = show ch
  show (LitString s) = show s
  show LitUnit = "()"