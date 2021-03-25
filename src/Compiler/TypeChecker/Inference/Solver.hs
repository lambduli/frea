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


bind :: String -> Type -> Solve Subst
bind varname type'
  | type' == TyVar varname  = return empty'subst
  | occurs varname type'    = throwError $ InfiniteType varname type'
  | otherwise               = return $ Sub $ Map.singleton varname type'
