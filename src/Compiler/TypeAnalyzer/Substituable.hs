{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.TypeAnalyzer.Substituable where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Constraint


-- | Substitution -- ordered mapping between name and a
newtype Subst a = Sub (Map.Map String a)
  deriving (Eq, Show)


class Substitutable a b where
  apply :: Subst b -> a -> a


class Term a where
  free'vars :: a -> Set.Set String


instance Substitutable Type Type where
  apply (Sub s) var@(TyVar varname kind')
    = Map.findWithDefault var varname s
  apply _ (TyCon conname kind')
    = TyCon conname kind'
  apply s (left `TyArr` right)
    = apply s left `TyArr` apply s right
  apply s (TyTuple types)
    = TyTuple $ map (apply s) types
  apply s (TyApp t'left t'right)
    = TyApp (apply s t'left) (apply s t'right)

  apply (Sub s) (TyOp par t'body)
    = TyOp par $ apply (Sub $ Map.delete par s) t'body


instance Term Type where
  free'vars type' = case type' of
    TyVar name kind' -> Set.singleton name
    TyCon name kind' -> Set.empty
    TyTuple ts -> foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty ts
    TyArr left right -> free'vars left `Set.union` free'vars right
    TyApp left right -> free'vars left `Set.union` free'vars right

    TyOp par body -> Set.delete par $ free'vars body


instance Substitutable Kind Kind where
  apply (Sub s) var@(KVar varname)
    = Map.findWithDefault var varname s
  apply s (left `KArr` right)
    = apply s left `KArr` apply s right
  apply _ Star
    = Star


instance Term Kind where
  free'vars Star = Set.empty
  free'vars (KArr left right) = free'vars left `Set.union` free'vars right 
  free'vars (KVar name) = Set.singleton name


instance Substitutable Scheme Type where
  apply (Sub s) (ForAll varnames type')
    = ForAll varnames $ apply s' type'
      where s' = Sub $ foldr Map.delete s varnames


instance Term Scheme where
  free'vars (ForAll vars type')
    = free'vars type' `Set.difference` Set.fromList vars


instance Substitutable a a => Substitutable (Constraint a) a where
  apply s (t'l, t'r)
    = (apply s t'l, apply s t'r)


instance Term a => Term (a, a) where
  free'vars (t'l, t'r)
    = free'vars t'l `Set.union` free'vars t'r


instance Substitutable a b => Substitutable [a] b where
  apply = map . apply


instance Term a => Term [a] where
  free'vars = foldr (Set.union . free'vars) Set.empty


instance Substitutable TypeEnv Type where
  apply subst type'env
    = Map.map
        (apply subst)
        type'env


instance Term TypeEnv where
  free'vars type'env
    = Map.foldr
        (\ scheme free'set -> free'set `Set.union` free'vars scheme)
        Set.empty
        type'env



instance Substitutable KindEnv Kind where
  apply subst kind'env
    = Map.map
        (apply subst)
        kind'env


instance Term KindEnv where
  free'vars kind'env
    = Map.foldr
        (\ kind' free'set -> free'set `Set.union` free'vars kind')
        Set.empty
        kind'env


empty'subst :: Subst a
empty'subst = Sub Map.empty