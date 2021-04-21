module Compiler.Syntax.Signature where

import Compiler.Syntax.Type (Type)


data Sig
  = TypeSig String Type


instance Show Sig where
  show (TypeSig name type')
    = name ++ " :: " ++ show type'
