module Compiler.Syntax.Bind where

import Compiler.Syntax (Sig, MatchGroup, Match)

data Bind
  = FunBind String Match