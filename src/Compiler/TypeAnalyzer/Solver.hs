{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.TypeAnalyzer.Solver where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Functor.Identity

import Control.Monad.Except

import Compiler.Syntax.Type
import Compiler.Syntax.Kind

import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.Constraint
import Compiler.TypeAnalyzer.AnalyzeEnv


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


run'solve :: ((Substitutable a a), (Unifiable a)) => [Constraint a] -> Either Error (Subst a)
run'solve constrs = runIdentity $ runExceptT $ solver state
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
  unify (TyVar (TVar v k')) t = v `bind` t
  unify t (TyVar (TVar v k')) = v `bind` t
  -- TODO: use the k's to make sure we are unifying only Type Variable of specific Kind with the Type of the same Kind

  unify (TyArr t1 t2) (TyArr t3 t4) = [t1, t2] `unify'many` [t3, t4]
  unify (TyApp t1 t2) (TyApp t3 t4) = [t1, t2] `unify'many` [t3, t4]
  unify l@(TyCon (TCon name'l k'l)) r@(TyCon (TCon name'r k'r))
    | name'l == name'r = return empty'subst
    | otherwise = throwError $ TypeUnifMismatch l r
    -- TODO: use the kinds to make sure we are unifying only Type Constants of the same Kind
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


-- {- symmetric merge function from the paper Typing Haskell in Haskell -}
-- merge :: (Substitutable a a, Monad m) => Subst a -> Subst a -> m (Subst a)
-- s1@(Sub sub'l) `merge` s2@(Sub sub'r)
--   = if agree then return (Sub $ sub'l `Map.union` sub'r) else fail "merge fails"
--     where
--       agree = all (\ var -> apply s1 (TyVar ))



instance Occurable Type where
  name `occurs'in` (TyVar (TVar varname k'))
    = name == varname
  name `occurs'in` (TyCon (TCon conname k'))
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
    -- | type' == TyVar varname Star    = return empty'subst
    -- TODO: FIX! the Star
    -- it's here just so it compiles
    -- BUT, instead I think I need to replace the varname String with Variable type or something like that
    -- which will have the name and the Kind
    -- then I will be able to compare it
    -- alternatively I can redefine it for now like:
    | TyVar (TVar name _) <- type', name == varname = return empty'subst
    | varname `occurs'in` type' = throwError $ InfiniteType (TyVar (TVar varname Star)) type'
    -- TODO: I think this is interesting!
    -- I don't think infinite type can have any particular Kind
    -- I think that infinite type doesn't have a proper kind in this system
    -- so some better way of reporting that error would be ideal.
    | otherwise                 = return $ Sub $ Map.singleton varname type'


instance Bindable Kind where
  bind varname kind'
    | kind' == KVar varname     = return empty'subst
    | varname `occurs'in` kind' = throwError $ InfiniteKind (KVar varname) kind'
    | otherwise                 = return $ Sub $ Map.singleton varname kind'
