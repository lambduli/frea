module Compiler.KindChecker.KindError where

import Data.List

import Compiler.Syntax.Kind
import Compiler.Syntax.Type


data KindError
  = InfiniteKind String Kind
  | UnifMismatch String String
  | UnboundVariable String
  | UnifShapeMismatch Kind Kind
  | UnifCountMismatch [Kind] [Kind]
  | SynonymCycle [(String, Type)]
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
  show (SynonymCycle aliases)
    = "[Kind] Found a cycle in the type synonym declaration(s) of\n" ++ intercalate "\n" (map prnt aliases)
      where
        prnt (name, type') = "  type " ++ name ++ " = " ++ show type'