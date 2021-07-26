{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.TypeAnalyzer.Solver where

import Data.List (intersect)
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


type Unifier k a = (Subst k a, [Constraint a])


class Unifiable k a where
  unify :: a -> a -> Solve (Subst k a)


class UnifiableComb k a b where
  unify'many :: a b -> a b -> Solve (Subst k b)


class Occurable a where
  occurs'in :: String -> a -> Bool


class Bindable k a where
  bind :: k -> a -> Solve (Subst k a)


class Composable k b where
  compose :: Subst k b -> Subst k b -> Subst k b
  merge :: (Monad m, MonadFail m) => Subst k b -> Subst k b -> m (Subst k b)


run'solve :: (Substitutable k a a, Unifiable k a, Composable k a) => [Constraint a] -> Either Error (Subst k a)
run'solve constrs = runIdentity $ runExceptT $ solver state
  where state = (empty'subst, constrs)


-- Unification solver
solver :: (Substitutable k a a, Unifiable k a, Composable k a) => Unifier k a -> Solve (Subst k a)
solver (subst, constraints) =
  case constraints of
    [] -> return subst
    ((type'l, type'r) : constrs) -> do
      subst'  <- type'l `unify` type'r
      solver (subst' `compose` subst, apply subst' constrs)


instance Unifiable TVar Type where
  unify t1 t2 | t1 == t2 = return empty'subst
  unify (TyVar var) t = var `bind` t
  unify t (TyVar var) = var `bind` t
  -- TODO: use the k's to make sure we are unifying only Type Variable of specific Kind with the Type of the same Kind

  -- unify (TyArr t1 t2) (TyArr t3 t4) = [t1, t2] `unify'many` [t3, t4]
  unify (TyApp t1 t2) (TyApp t3 t4) = [t1, t2] `unify'many` [t3, t4]
  unify l@(TyCon t'con'l) r@(TyCon t'con'r)
    | t'con'l == t'con'r = return empty'subst
    | otherwise = throwError $ TypeUnifMismatch l r
    -- TODO: use the kinds to make sure we are unifying only Type Constants of the same Kind
  unify (TyTuple ts'left) (TyTuple ts'right)
    = if length ts'left /= length ts'right
      then throwError $ TypeShapeMismatch (TyTuple ts'left) (TyTuple ts'right)
      else ts'left `unify'many` ts'right
  unify t1 t2 = throwError $ TypeShapeMismatch t1 t2


instance Unifiable String Kind where
  unify t1 t2 | t1 == t2 = return empty'subst
  unify (KVar v) k = v `bind` k
  unify k (KVar v) = v `bind` k
  unify (KArr k1 k2) (KArr k3 k4) = [k1, k2] `unify'many` [k3, k4]
  unify Star Star = return empty'subst
  unify t1 t2 = throwError $ KindShapeMismatch t1 t2


instance UnifiableComb TVar [] Type where
  unify'many [] [] = return empty'subst
  unify'many (t'l : ts'l) (t'r : ts'r) = do
    su1 <- t'l `unify` t'r
    su2 <- apply su1 ts'l `unify'many` apply su1 ts'r
    return (su2 `compose` su1)
  unify'many t'l t'r = throwError $ TypeUnifCountMismatch t'l t'r


instance UnifiableComb String [] Kind where
  unify'many [] [] = return empty'subst
  unify'many (t'l : ts'l) (t'r : ts'r) = do
    su1 <- t'l `unify` t'r
    su2 <- apply su1 ts'l `unify'many` apply su1 ts'r
    return (su2 `compose` su1)
  unify'many t'l t'r = throwError $ KindUnifCountMismatch t'l t'r


instance Composable TVar Type where
  -- compose :: (Substitutable k a a, Ord k) => Subst k a -> Subst k a -> Subst k a
  (Sub sub'l) `compose` (Sub sub'r)
    = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l

  -- {- symmetric merge function from the paper Typing Haskell in Haskell -}
  -- merge :: (Substitutable TVar a a, Monad m) => Subst TVar a -> Subst TVar a -> m (Subst TVar a)
  s1@(Sub sub'l) `merge` s2@(Sub sub'r)
    = if agree then return (Sub $ sub'l `Map.union` sub'r) else fail "merge fails"
      where
        agree = all (\ var -> apply s1 (TyVar var) == apply s2 (TyVar var)) (Map.keys sub'l `intersect` Map.keys sub'r)


instance Composable String Kind where
  (Sub sub'l) `compose` (Sub sub'r)
    = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l

  s1@(Sub sub'l) `merge` s2@(Sub sub'r)
    = if agree then return (Sub $ sub'l `Map.union` sub'r) else fail "merge fails"
      where
        agree = all (\ var -> apply s1 (KVar var) == apply s2 (KVar var)) (Map.keys sub'l `intersect` Map.keys sub'r)


{- NOTE:  This piece of code raises a question.
          Is it possible to replace the whole Occurable class with it's single method `occurs'in`
          with just calling free'vars of Term and then testing whether your thing is the member of the set?

          It would seem that depends on whether that specific behaviour for the TyCon is used and important.
          I *think* it is used for type synonym expansion now. (Type synonym is tested for presence in the type expression and so on.)
          But that doesn't necessary mean it has to stay. I will need to refactor type synonym implementation in the future.
          Since mine allows for more expressive type expressions to be constructed (partially applied type synonyms / functions).
          I as we know, this has a potential to break decidability. So I will get rid of it and proceed same as GHC.
-}
instance Occurable Type where
  name `occurs'in` (TyVar (TVar varname k'))
    = name == varname
  name `occurs'in` (TyCon (TCon conname k'))
    = name == conname -- TODO: So I think I can do that safely. Consider if TyCon didn't exist and everything would just be a TyVar. You would do this check and by the fact that constructors start with upper case letter it wouldn't break anything. 
  name `occurs'in` (TyTuple ts)
    = any (name `occurs'in`) ts
  -- name `occurs'in` (TyArr left right)
  --   = name `occurs'in` left || name `occurs'in` right
  name `occurs'in` (TyApp left right)
    = name `occurs'in` left || name `occurs'in` right


instance Occurable Kind where
  name `occurs'in` (KVar varname)
    = name == varname
  name `occurs'in` Star
    = False
  name `occurs'in` (KArr left right)
    = name `occurs'in` left || name `occurs'in` right


instance Bindable TVar Type where
  bind var@(TVar name _) type'
    -- | type' == TyVar varname Star    = return empty'subst
    -- TODO: FIX! the Star
    -- it's here just so it compiles
    -- BUT, instead I think I need to replace the varname String with Variable type or something like that
    -- which will have the name and the Kind
    -- then I will be able to compare it
    -- alternatively I can redefine it for now like:
    | TyVar var == type' = return empty'subst
    | name `occurs'in` type' = throwError $ InfiniteType (TyVar var) type'
    -- TODO: I *SHOULD* consider refactoring `occurs'in` so it takes parametrized argument
    -- in this case I would invoke it with TVar
    --
    -- TODO: I think this is interesting!
    -- I don't think infinite type can have any particular Kind
    -- I think that infinite type doesn't have a proper kind in this system
    -- so some better way of reporting that error would be ideal.
    | otherwise                 = return $ Sub $ Map.singleton var type'


instance Bindable String Kind where
  bind varname kind'
    | kind' == KVar varname     = return empty'subst
    | varname `occurs'in` kind' = throwError $ InfiniteKind (KVar varname) kind'
    | otherwise                 = return $ Sub $ Map.singleton varname kind'
