{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.KindChecker.Substituable where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Compiler.Syntax.Kind
import Compiler.KindChecker.Constraint
import Compiler.KindChecker.KindEnv


-- type substitution -- ordered mapping between name and kind
newtype Subst = Sub (Map.Map String Kind)
  deriving (Eq, Show)


class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set String


instance Substitutable Kind where
  apply (Sub s) var@(KVar varname)
    = Map.findWithDefault var varname s
  apply s (left `KArr` right)
    = apply s left `KArr` apply s right
  apply _ Star
    = Star

  ftv kind' = case kind' of
    Star -> Set.empty
    KArr left right -> ftv left `Set.union` ftv right
    KVar name -> Set.singleton name


instance Substitutable Constraint where
  apply s (t'l, t'r)
    = (apply s t'l, apply s t'r)

  ftv (t'l, t'r)
    = ftv t'l `Set.union` ftv t'r


instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty


instance Substitutable KindEnv where
  apply subst kind'env
    = Map.map
        (apply subst)
        kind'env

  ftv kind'env
    = Map.foldr
        (\ kind' free'set -> free'set `Set.union` ftv kind')
        Set.empty
        kind'env


empty'subst :: Subst
empty'subst = Sub Map.empty
