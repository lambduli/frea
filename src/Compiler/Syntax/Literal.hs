module Compiler.Syntax.Literal where


data Lit
  = LitInt Int
  | LitDouble Double
  | LitChar Char
  deriving (Eq, Ord)
  -- I can safely derive and use Ord because the type of Freas comparision operations
  -- prohibits from comparing two things of different types

instance Show Lit where
  show (LitInt i) = show i
  show (LitDouble d) = show d
  show (LitChar ch) = show ch