module Syntax.Bind where

import Syntax (Sig, MatchGroup, Match)

data Bind
  = FunBind String Match