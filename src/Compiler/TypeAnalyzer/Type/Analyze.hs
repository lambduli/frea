{-# LANGUAGE TupleSections #-}
module Compiler.TypeAnalyzer.Type.Analyze where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Functor.Identity
import Data.Bifunctor (second)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.Syntax.Literal

import Compiler.TypeAnalyzer.Types
import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.Solver
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Constraint
import Compiler.TypeAnalyzer.AnalyzeState
import Compiler.TypeAnalyzer.AnalyzeUtils

import qualified Compiler.TypeAnalyzer.Kind.Infer as K


-- | TODO: once I merge the kind checking into the same process as type checking
-- | I will be able to create two types of constraints, for types and for kinds
-- | that will allow me easily assert that some specific type should be of some specific kind
check :: Type -> Expression -> Analyze ((), [Constraint Type], [Constraint Kind])
-- check (TyVar n) e@(Lit _) = undefined
check t'Int (Lit (LitInt i)) = return ((), [], [])
check t'Double (Lit (LitDouble i)) = return ((), [], [])
check t'Char (Lit (LitChar i)) = return ((), [], [])

check t (Var x) = do
  -- for now just assume t is valid type of kind *
  type' <- lookup't'env x
  return ((), [(t, type')], [])

check t (Op x) = do
  -- assume t :: *
  type' <- lookup't'env x
  return ((), [(t, type')], [])

check (from `TyArr` to) (Lam x body) = do
  -- assume from :: * and to :: *
  (t, constrs, k'constrs) <- put'in't'env (x, ForAll [] from) (check to body)
  return ((), constrs, k'constrs)

check t (App left right) = do
  -- assume t :: *
  (t'l, cs'l, k'cs'l) <- infer left
  (t'r, cs'r, k'cs'r) <- infer right
  fresh'name <- fresh 
  let t'var = TyVar fresh'name
  return ((), (t, t'var) : (t'l, t'r `TyArr` t'var) : cs'l ++ cs'r, k'cs'l ++ k'cs'r)

check t (If cond tr fl) = do
  -- assume t :: *
  (t1, c1, k'c1) <- infer cond
  ((), c2, k'c2) <- check t tr
  ((), c3, k'c3) <- check t fl
  return ((), (t1, t'Bool) : c1 ++ c2 ++ c3, k'c1 ++ k'c2 ++ k'c3)

check t (Let x ex'val ex'body) = do
  -- assume t :: *
  (_, t'env, _) <- ask
  (t'val, cs'val, k'cs'val) <- infer ex'val
  case runSolve cs'val of
      Left err -> throwError err
      Right sub -> do
          let sc = generalize (apply sub t'env) (apply sub t'val)
          ((), cs'body, k'cs'body) <- put'in't'env (x, sc) $ local (\ (a, b, c) -> (a, apply sub b, c)) (check t ex'body)
          return ((), cs'val ++ cs'body, k'cs'val ++ k'cs'body) --  ^^^ terrible : TODO: fix pls

check (TyTuple types') (Tuple exprs) = do
  -- assume each type :: * where type isfrom types'
  (cs, k'cs) <- foldM check' ([], []) (zip types' exprs)
  return ((), cs, k'cs)
    where
      check' (constrs, k'constrs) (ty, expr) = do
        ((), cs, k'cs) <- check ty expr
        return (cs ++ constrs, k'constrs ++ k'cs)

check _ (Fix _) = throwError $ Unexpected "I am not type checking Fix expressions right now."


infer :: Expression -> Analyze (Type, [Constraint Type], [Constraint Kind])
infer expr = case expr of  
  Lit (LitInt i) -> return (t'Int, [], [])
  Lit (LitDouble d) -> return (t'Double, [], [])
  Lit (LitChar ch) -> return (t'Char, [], [])

  (Var x) -> do
    type' <- lookup't'env x
    return (type', [], [])

  Op x -> do
    type' <- lookup't'env x
    return (type', [], [])

  Lam x body -> do
    fresh'name <- fresh
    let t'var = TyVar fresh'name
    (t, t'constrs, k'constrs) <- put'in't'env (x, ForAll [] t'var) (infer body)
    return (t'var `TyArr` t, t'constrs, k'constrs)

  App left right -> do
    (t'l, cs'l, k'cs'l) <- infer left
    (t'r, cs'r, k'cs'r) <- infer right
    fresh'name <- fresh
    let t'var = TyVar fresh'name
    return (t'var, cs'l ++ cs'r ++ [(t'l, t'r `TyArr` t'var)], k'cs'l ++ k'cs'r)

  If cond tr fl -> do
    (t1, c1, k'c1) <- infer cond
    (t2, c2, k'c2) <- infer tr
    (t3, c3, k'c3) <- infer fl
    return (t2, (t1, t'Bool) : (t2, t3) : c1 ++ c2 ++ c3, k'c1 ++ k'c2 ++ k'c3)
  
  Let x ex'val ex'body -> do
    (_, t'env, _) <- ask
    (t'val, cs'val, k'cs'val) <- infer ex'val
    case runSolve cs'val of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (apply sub t'env) (apply sub t'val)
            (t'body, cs'body, k'cs'body) <- put'in't'env (x, sc) $ local (\ (a, b, c) -> (a, apply sub b, c)) (infer ex'body)
            return (t'body, cs'val ++ cs'body, k'cs'val ++ k'cs'body) --  ^^^ terrible : TODO: fix pls

  Fix expr -> do
    (type', cs, k'cs) <- infer expr
    fresh'name <- fresh
    let t'var = TyVar fresh'name
    return (t'var, cs ++ [(t'var `TyArr` t'var, type')], k'cs)

  Tuple exprs -> do
    (types, cs, k'cs) <- foldM infer' ([], [], []) exprs
    return (TyTuple $ reverse types, cs, k'cs)
      where
        infer' (types, constrs, k'constrs) expr = do
          (t, cs, k'cs) <- infer expr
          return (t : types, cs ++ constrs, k'cs ++ k'constrs)
  
  Ann type' expr -> do
    let scheme = generalize empty't'env type'
    t' <- instantiate scheme
    pairs <- mapM (\ name -> (name,) <$> (KVar <$> fresh)) (Set.toList $ free'vars t')
    (kind, k'constrs) <- merge'into'k'env pairs (K.infer t')
    (_, constrs, k'cs) <- check t' expr
    return (t', constrs, (kind, Star) : k'constrs ++ k'cs)
