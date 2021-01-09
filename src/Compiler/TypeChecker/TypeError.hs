module Compiler.TypeChecker.TypeError where


import Compiler.Syntax (Type(..), Scheme(..))

data TypeError
  = InfiniteType String Type
  | UnifMismatch String String
  | UnboundVariable String
  | UnboundConstructor String
  | UnifShapeMismatch Type Type
  deriving (Show)
