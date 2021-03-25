module Compiler.TypeChecker.TypeError where


import Compiler.Syntax (Type(..), Scheme(..))

data TypeError
  = InfiniteType String Type
  | UnifMismatch String String
  | UnboundVariable String
  | UnifShapeMismatch Type Type
  | UnifCountMismatch [Type] [Type]
  deriving (Eq)

instance Show TypeError where
  show (InfiniteType name type')
    = "Occurs check: cannot constructthe infinite type:\n  "
      ++ name ++ " ~ " ++ show type'
  show (UnifMismatch name'l name'r)
    = "Couldn't match type `" ++ name'l ++ "` with `" ++ name'r ++ "`"
  show (UnboundVariable name)
    = "Unknown variable " ++ name
  show (UnifShapeMismatch type'l type'r)
    = "[Shape] Couldn't match type `" ++ show type'l ++ "` with `" ++ show type'r ++ "`"
  show (UnifCountMismatch left right)
    = "[Count] Couldn't unify  " ++ show left ++ " ~/~  " ++ show right
