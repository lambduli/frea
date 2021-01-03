{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Syntax.MatchGroup where

import Compiler.Syntax (Pattern, Expression)


data MatchGroup = MG [Match]
  deriving (Show)


data Match = Match
  { matchPat  :: Pattern
  , rhs       :: Expression }
  deriving (Show)
