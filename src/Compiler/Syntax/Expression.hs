{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Syntax.Expression where

import Compiler.Syntax (Lit, Type, MatchGroup)

data Expression
  = Var String
  | Con String
  | Lit Lit
  | Lam [String] Expression
  | App Expression Expression
  | NegApp Expression -- syntactic negation -- think about it
  | MatchWith Expression MatchGroup -- OK
  | If Expression Expression Expression -- OK
  | Let String Expression Expression -- OK
  | Typed Type Expression -- OK
