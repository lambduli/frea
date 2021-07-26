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
newtype Subst k v = Sub (Map.Map k v)
  deriving (Eq, Show)


class Substitutable k t b where
  apply :: Subst k b -> t -> t


class Ord k => Term k t where
  free'vars :: t -> Set.Set k


instance Substitutable TVar Type Type where
  apply (Sub s) var@(TyVar t'var)
    = Map.findWithDefault var t'var s
  apply _ (TyCon ty'con)
    = TyCon ty'con
  apply s (TyApp t'left t'right)
    = TyApp (apply s t'left) (apply s t'right)
  -- apply s (left `TyArr` right)
    -- = apply s left `TyArr` apply s right
  apply s (TyTuple types)
    = TyTuple $ map (apply s) types

  apply (Sub s) (TyOp par t'body)
    = TyOp par $ apply (Sub $ Map.filterWithKey (\ (TVar name _) _ -> name /= par) s) t'body
    -- original line: = TyOp par $ apply (Sub $ Map.delete par s) t'body
    -- I changed it so it works with Map TVar Type (a new Substitution representation)


instance Term TVar Type where
  free'vars type' = case type' of
    TyVar t'var -> Set.singleton t'var
    TyCon (TCon name kind') -> Set.empty
    TyApp left right -> free'vars left `Set.union` free'vars right
    TyTuple ts -> foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty ts
    -- TyArr left right -> free'vars left `Set.union` free'vars right

    TyOp par body -> Set.filter (\ (TVar name _) -> name /= par) $ free'vars body
      -- original line: Set.delete par $ free'vars body
      -- I changed it so it works with Set TVar



instance Substitutable String Kind Kind where
  apply (Sub s) var@(KVar varname)
    = Map.findWithDefault var varname s
  apply s (left `KArr` right)
    = apply s left `KArr` apply s right
  apply _ Star
    = Star


instance Term String Kind where
  free'vars Star = Set.empty
  free'vars (KArr left right) = free'vars left `Set.union` free'vars right 
  free'vars (KVar name) = Set.singleton name


instance Substitutable TVar Scheme Type where
  apply (Sub s) (ForAll varnames type')
    = ForAll varnames $ apply s' type'
      where s' = Sub $ foldr Map.delete s varnames


instance Term TVar Scheme where
  free'vars (ForAll vars type')
    = free'vars type' `Set.difference` Set.fromList vars


instance Substitutable k a a => Substitutable k (Constraint a) a where
  apply s (t'l, t'r)
    = (apply s t'l, apply s t'r)


instance Term k a => Term k (a, a) where
  free'vars (t'l, t'r)
    = free'vars t'l `Set.union` free'vars t'r


instance Substitutable k a b => Substitutable k [a] b where
  apply = map . apply


instance Term k a => Term k [a] where
  free'vars = foldr (Set.union . free'vars) Set.empty


instance Substitutable TVar TypeEnv Type where
  apply subst type'env
    = Map.map
        (apply subst)
        type'env


instance Term TVar TypeEnv where
  free'vars type'env
    = Map.foldr
        (\ scheme free'set -> free'set `Set.union` free'vars scheme)
        Set.empty
        type'env



instance Substitutable String KindEnv Kind where
  apply subst kind'env
    = Map.map
        (apply subst)
        kind'env


instance Term String KindEnv where
  free'vars kind'env
    = Map.foldr
        (\ kind' free'set -> free'set `Set.union` free'vars kind')
        Set.empty
        kind'env


empty'subst :: Subst k a
empty'subst = Sub Map.empty