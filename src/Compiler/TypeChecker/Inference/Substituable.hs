module Compiler.TypeChecker.Inference.Substituable where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Compiler.Syntax.Type


-- type substitution -- ordered mapping between name and type
newtype Subst = Sub (Map.Map String Type)
  deriving (Eq, Show)


class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set String
