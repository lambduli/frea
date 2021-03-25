{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.TypeChecker.Inference.Substituable where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Compiler.Syntax.Type
import Compiler.TypeChecker.Inference.Constraint
import Compiler.TypeChecker.Inference.TypeEnv


-- type substitution -- ordered mapping between name and type
newtype Subst = Sub (Map.Map String Type)
  deriving (Eq, Show)


class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set String


instance Substitutable Type where
  apply (Sub s) var@(TyVar varname)
    = Map.findWithDefault var varname s
  apply _ (TyCon conname)
    = TyCon conname
  apply s (left `TyArr` right)
    = apply s left `TyArr` apply s right
  apply s (TyTuple types)
    = TyTuple $ map (apply s) types
  apply s (TyList type')
    = TyList $ apply s type'

  ftv type' = case type' of
    TyVar name -> Set.singleton name
    TyCon name -> Set.empty
    TyTuple ts -> foldl (\ set' t' -> Set.union set' (ftv t')) Set.empty ts
    TyList t -> ftv t
    TyArr left right -> ftv left `Set.union` ftv right


instance Substitutable Scheme where
  apply (Sub s) (ForAll varnames type')
    = ForAll varnames $ apply s' type'
      where s' = Sub $ foldr Map.delete s varnames

  ftv (ForAll vars type')
    = ftv type' `Set.difference` Set.fromList vars


instance Substitutable Constraint where
  apply s (t'l, t'r)
    = (apply s t'l, apply s t'r)

  ftv (t'l, t'r)
    = ftv t'l `Set.union` ftv t'r


instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty


instance Substitutable TypeEnv where
  apply subst (Env type'env)
    = Env $ Map.map
        (apply subst)
        type'env

  ftv (Env type'env)
    = Map.foldr
        (\ scheme free'set -> free'set `Set.union` ftv scheme)
        Set.empty
        type'env


empty'subst :: Subst
empty'subst = Sub Map.empty
