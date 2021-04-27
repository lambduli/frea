module Compiler.TypeChecker.Error where

import Compiler.Syntax (Type, Scheme, Kind)


data Error
  = InfiniteType Type Type
  | InfiniteKind Kind Kind
  | TypeUnifMismatch Type Type
  | KindUnifMismatch Kind Kind
  | UnboundVar String
  | TypeShapeMismatch Type Type
  | KindShapeMismatch Kind Kind
  | TypeUnifCountMismatch [Type] [Type]
  | KindUnifCountMismatch [Kind] [Kind]
  | SynonymCycle [(String, Type)]

  | Unexpected String