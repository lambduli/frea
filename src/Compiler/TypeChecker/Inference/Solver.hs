module Compiler.TypeChecker.Inference.Solver where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Data.Functor.Identity

import Compiler.Syntax.Type
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.Substituable
import Compiler.TypeChecker.Inference.Constraint
import Compiler.TypeChecker.Inference.TypeEnv



-- Constraint solver monad
type Solve a = ExceptT TypeError Identity a

type Unifier = (Subst, [Constraint])


runSolve :: [Constraint] -> Either TypeError Subst
runSolve constrs = runIdentity $ runExceptT $ solver state
  where state = (empty'subst, constrs)


-- Unification solver
solver :: Unifier -> Solve Subst
solver (subst, constraints) =
  case constraints of
    [] -> return subst
    ((type'l, type'r) : constrs) -> do
      subst'  <- type'l `unify` type'r
      solver (subst' `compose` subst, apply subst' constrs)


unify :: Type -> Type -> Solve Subst
unify t1 t2 | t1 == t2 = return empty'subst
unify (TyVar v) t = v `bind` t
unify t (TyVar v) = v `bind` t
unify (TyArr t1 t2) (TyArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unify (TyCon name'l) (TyCon name'r)
  | name'l == name'r = return empty'subst
  | otherwise = throwError $ UnifMismatch name'l name'r
unify (TyTuple ts'left) (TyTuple ts'right)
  = if length ts'left /= length ts'right
    then throwError $ UnifShapeMismatch (TyTuple ts'left) (TyTuple ts'right)
    else unifyMany ts'left ts'right
unify (TyList t'left) (TyList t'right)
  = unify t'left t'right
unify t1 t2 = throwError $ UnifShapeMismatch t1 t2


unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return empty'subst
unifyMany (t'l : ts'l) (t'r : ts'r) = do
  su1 <- t'l `unify` t'r
  su2 <- unifyMany (apply su1 ts'l) (apply su1 ts'r)
  return (su2 `compose` su1)
unifyMany t'l t'r = throwError $ UnifCountMismatch t'l t'r


compose :: Subst -> Subst -> Subst
(Sub sub'l) `compose` (Sub sub'r)
  = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l


occurs'in :: String -> Type -> Bool
name `occurs'in` (TyVar varname)
  = name == varname
name `occurs'in` (TyCon conname)
  = False
name `occurs'in` (TyTuple ts)
  = any (name `occurs'in`) ts
name `occurs'in` (TyList t)
  = name `occurs'in` t
name `occurs'in` (TyArr left right)
  = name `occurs'in` left || name `occurs'in` right


bind :: String -> Type -> Solve Subst
bind varname type'
  | type' == TyVar varname    = return empty'subst
  | varname `occurs'in` type' = throwError $ InfiniteType varname type'
  | otherwise                 = return $ Sub $ Map.singleton varname type'
