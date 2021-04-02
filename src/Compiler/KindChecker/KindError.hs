module Compiler.KindChecker.KindError where

import Compiler.Syntax.Kind


data KindError
  = InfiniteKind String Kind
  | UnifMismatch String String
  | UnboundVariable String
  | UnifShapeMismatch Kind Kind
  | UnifCountMismatch [Kind] [Kind]
  deriving (Eq)

instance Show KindError where
  show (InfiniteKind name kind')
    = "[Kind] Occurs check: cannot construct the infinite kind:\n  "
      ++ name ++ " ~ " ++ show kind'
  show (UnifMismatch name'l name'r)
    = "[Kind] Couldn't match kind `" ++ name'l ++ "` with `" ++ name'r ++ "`"
  show (UnboundVariable name)
    = "Unknown type variable " ++ name
  show (UnifShapeMismatch kind'l kind'r)
    = "[Kind][Shape] Couldn't match kind `" ++ show kind'l ++ "` with `" ++ show kind'r ++ "`"
  show (UnifCountMismatch left right)
    = "[Kind][Count] Couldn't unify  " ++ show left ++ " ~/~  " ++ show right