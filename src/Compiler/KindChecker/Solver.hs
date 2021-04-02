module Compiler.KindChecker.Solver where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Data.Functor.Identity

import Compiler.Syntax.Kind
import Compiler.KindChecker.KindError
import Compiler.KindChecker.Infer
import Compiler.KindChecker.Substituable
import Compiler.KindChecker.Constraint
import Compiler.KindChecker.KindEnv


-- Constraint solver monad
type Solve a = ExceptT KindError Identity a


type Unifier = (Subst, [Constraint])


runSolve :: [Constraint] -> Either KindError Subst
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


unify :: Kind -> Kind -> Solve Subst
unify t1 t2 | t1 == t2 = return empty'subst
unify (KVar v) k = v `bind` k
unify k (KVar v) = v `bind` k
unify (KArr k1 k2) (KArr k3 k4) = unifyMany [k1, k2] [k3, k4]
unify Star Star = return empty'subst
unify t1 t2 = throwError $ UnifShapeMismatch t1 t2


unifyMany :: [Kind] -> [Kind] -> Solve Subst
unifyMany [] [] = return empty'subst
unifyMany (k'l : ks'l) (k'r : ks'r) = do
  su1 <- k'l `unify` k'r
  su2 <- unifyMany (apply su1 ks'l) (apply su1 ks'r)
  return (su2 `compose` su1)
unifyMany k'l k'r = throwError $ UnifCountMismatch k'l k'r


compose :: Subst -> Subst -> Subst
(Sub sub'l) `compose` (Sub sub'r)
  = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l


occurs'in :: String -> Kind -> Bool
name `occurs'in` (KVar varname)
  = name == varname
name `occurs'in` Star
  = False
name `occurs'in` (KArr left right)
  = name `occurs'in` left || name `occurs'in` right


bind :: String -> Kind -> Solve Subst
bind varname kind'
  | kind' == KVar varname     = return empty'subst
  | varname `occurs'in` kind' = throwError $ InfiniteKind varname kind'
  | otherwise                 = return $ Sub $ Map.singleton varname kind'
