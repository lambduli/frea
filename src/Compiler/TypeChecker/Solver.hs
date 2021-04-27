{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.TypeChecker.Solver where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Functor.Identity

import Control.Monad.Except

import Compiler.Syntax.Type
import Compiler.Syntax.Kind

import Compiler.TypeChecker.Error
import Compiler.TypeChecker.Analize
import Compiler.TypeChecker.Substituable
import Compiler.TypeChecker.Constraint
import Compiler.TypeChecker.AnalizeEnv


-- Constraint solver monad
type Solve a = ExceptT Error Identity a


type Unifier a = (Subst a, [Constraint a])


class Unifiable a where
  unify :: a -> a -> Solve (Subst a)


class UnifiableComb a b where
  unify'many :: a b -> a b -> Solve (Subst b)


class Occurable a where
  occurs'in :: String -> a -> Bool


class Bindable a where
  bind :: String -> a -> Solve (Subst a)


runSolve :: ((Substitutable a a), (Unifiable a)) => [Constraint a] -> Either Error (Subst a)
runSolve constrs = runIdentity $ runExceptT $ solver state
  where state = (empty'subst, constrs)


-- Unification solver
solver :: ((Substitutable a a), (Unifiable a)) => Unifier a -> Solve (Subst a)
solver (subst, constraints) =
  case constraints of
    [] -> return subst
    ((type'l, type'r) : constrs) -> do
      subst'  <- type'l `unify` type'r
      solver (subst' `compose` subst, apply subst' constrs)


instance Unifiable Type where
  unify t1 t2 | t1 == t2 = return empty'subst
  unify (TyVar v) t = v `bind` t
  unify t (TyVar v) = v `bind` t
  unify (TyArr t1 t2) (TyArr t3 t4) = [t1, t2] `unify'many` [t3, t4]
  unify (TyApp t1 t2) (TyApp t3 t4) = [t1, t2] `unify'many` [t3, t4]
  unify l@(TyCon name'l) r@(TyCon name'r)
    | name'l == name'r = return empty'subst
    | otherwise = throwError $ TypeUnifMismatch l r
  unify (TyTuple ts'left) (TyTuple ts'right)
    = if length ts'left /= length ts'right
      then throwError $ TypeShapeMismatch (TyTuple ts'left) (TyTuple ts'right)
      else ts'left `unify'many` ts'right
  unify t1 t2 = throwError $ TypeShapeMismatch t1 t2


instance Unifiable Kind where
  unify t1 t2 | t1 == t2 = return empty'subst
  unify (KVar v) k = v `bind` k
  unify k (KVar v) = v `bind` k
  unify (KArr k1 k2) (KArr k3 k4) = [k1, k2] `unify'many` [k3, k4]
  unify Star Star = return empty'subst
  unify t1 t2 = throwError $ KindShapeMismatch t1 t2


instance UnifiableComb [] Type where
  unify'many [] [] = return empty'subst
  unify'many (t'l : ts'l) (t'r : ts'r) = do
    su1 <- t'l `unify` t'r
    su2 <- apply su1 ts'l `unify'many` apply su1 ts'r
    return (su2 `compose` su1)
  unify'many t'l t'r = throwError $ TypeUnifCountMismatch t'l t'r


instance UnifiableComb [] Kind where
  unify'many [] [] = return empty'subst
  unify'many (t'l : ts'l) (t'r : ts'r) = do
    su1 <- t'l `unify` t'r
    su2 <- apply su1 ts'l `unify'many` apply su1 ts'r
    return (su2 `compose` su1)
  unify'many t'l t'r = throwError $ KindUnifCountMismatch t'l t'r


compose :: Substitutable a a => Subst a -> Subst a -> Subst a
(Sub sub'l) `compose` (Sub sub'r)
  = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l


instance Occurable Type where
  name `occurs'in` (TyVar varname)
    = name == varname
  name `occurs'in` (TyCon conname)
    = name == conname -- TODO: So I think I can do that safely. Consider if TyCon didn't exist and everything would just be a TyVar. You would do this check and by the fact that constructors start with upper case letter it wouldn't break anything. 
  name `occurs'in` (TyTuple ts)
    = any (name `occurs'in`) ts
  name `occurs'in` (TyArr left right)
    = name `occurs'in` left || name `occurs'in` right
  name `occurs'in` (TyApp left right)
    = name `occurs'in` left || name `occurs'in` right


instance Occurable Kind where
  name `occurs'in` (KVar varname)
    = name == varname
  name `occurs'in` Star
    = False
  name `occurs'in` (KArr left right)
    = name `occurs'in` left || name `occurs'in` right


instance Bindable Type where
  bind varname type'
    | type' == TyVar varname    = return empty'subst
    | varname `occurs'in` type' = throwError $ InfiniteType (TyVar varname) type'
    | otherwise                 = return $ Sub $ Map.singleton varname type'


instance Bindable Kind where
  bind varname kind'
    | kind' == KVar varname     = return empty'subst
    | varname `occurs'in` kind' = throwError $ InfiniteKind (KVar varname) kind'
    | otherwise                 = return $ Sub $ Map.singleton varname kind'
