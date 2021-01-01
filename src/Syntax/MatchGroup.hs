{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Syntax.MatchGroup where

import Syntax (Pattern, Expression)


data MatchGroup = MG [Match]


data Match = Match
  { matchPat  :: Pattern
  , rhs       :: Expression }
