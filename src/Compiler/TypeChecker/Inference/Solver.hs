{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.TypeChecker.Inference.Solver where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Data.Functor.Identity

import Compiler.Syntax.Type
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.Substituable



-- Constraint solver monad
type Solve a = ExceptT TypeError Identity a

type Unifier = (Subst, [Constraint])


-- type substitution -- ordered mapping between name and type



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


-- Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve constrs = runIdentity $ runExceptT $ solver state
  where state = (empty'subst, constrs)


-- Unification solver
solver :: Unifier -> Solve Subst
solver (subst, constraints) =
  case constraints of
    [] -> return subst
    ((type'l, type'r) : constrs) -> do
      subst'  <- unifies type'l type'r
      solver (subst' `compose'subst` subst, apply subst' constrs)


unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return empty'subst
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies (TyArr t1 t2) (TyArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TyCon name'l) (TyCon name'r)
  | name'l == name'r = return empty'subst
  | otherwise = throwError $ UnifMismatch name'l name'r
unifies (TyTuple ts'left) (TyTuple ts'right)
  = if length ts'left /= length ts'right
    then throwError $ UnifShapeMismatch (TyTuple ts'left) (TyTuple ts'right)
    else unifyMany ts'left ts'right
unifies (TyList t'left) (TyList t'right)
  = unifies t'left t'right
unifies t1 t2 = throwError $ UnifShapeMismatch t1 t2


unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return empty'subst
unifyMany (t'l : ts'l) (t'r : ts'r) = do
  su1 <- unifies t'l t'r
  su2 <- unifyMany (apply su1 ts'l) (apply su1 ts'r)
  return (su2 `compose'subst` su1)
unifyMany t'l t'r = throwError $ UnifCountMismatch t'l t'r


-- Compose substitutions
compose'subst :: Subst -> Subst -> Subst
(Sub sub'l) `compose'subst` (Sub sub'r)
  = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l


occurs :: String -> Type -> Bool
occurs name (TyVar varname)
  = name == varname
occurs name (TyCon conname)
  = False
occurs name (TyTuple ts)
  = any (occurs name) ts
occurs name (TyList t)
  = occurs name t
occurs name (TyArr left right)
  = occurs name left || occurs name right


-- myslim ze tohle musim zmenit na :: ... -> Solve Unifier
-- return Unifier taky funguje
bind :: String -> Type -> Solve Subst
bind varname type'
  | type' == TyVar varname  = return empty'subst
  | occurs varname type'    = throwError $ InfiniteType varname type'
  | otherwise               = return $ Sub $ Map.singleton varname type'