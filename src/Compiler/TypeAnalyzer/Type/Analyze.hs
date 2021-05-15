{-# LANGUAGE TupleSections #-}
module Compiler.TypeAnalyzer.Type.Analyze where


import Data.Graph (SCC(..), stronglyConnComp)

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
import Compiler.TypeAnalyzer.Dependency


import qualified Compiler.TypeAnalyzer.Kind.Infer as K


check :: Type -> Expression -> Analyze ((), [Constraint Type], [Constraint Kind])
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

-- TODO: don't forget to fix this
-- check t (Let x ex'val ex'body) = do
--   -- assume t :: *
--   t'env <- asks type'env
--   (t'val, cs'val, k'cs'val) <- infer ex'val
--   case run'solve cs'val of
--       Left err -> throwError err
--       Right sub -> do
--           let sc = generalize (apply sub t'env) (apply sub t'val)
--           ((), cs'body, k'cs'body) <- put'in't'env (x, sc) $ local (\ e@AEnv{ type'env = t'env } -> e{ type'env = apply sub t'env }) (check t ex'body)
--           return ((), cs'val ++ cs'body, k'cs'val ++ k'cs'body)

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
  
  Let bind'pairs ex'body -> do
    -- TODO: do the same thing as for the global bindings
    -- first do the dependency analysis - this time though - only mark something as a dependency if it's one of the names in the let bindings
    -- second collect the constraints for all the Strongly Connected Components
    -- then solve all the constraints and obtain a lot's of type schemes
    -- put those in the typing context and finally infer ex'body
    -- TODO: check everything and remove commented out code
    (type'bindings, t'constrs, k'constrs) <- infer'many bind'pairs
    case run'solve t'constrs of
      Left err -> throwError err
      Right subst -> do
        t'env <- asks type'env
        let scheme'bindings = map (second (closeOver . apply subst)) type'bindings
            t'env' = apply subst $ t'env `Map.union` Map.fromList scheme'bindings
        
        (t'body, cs'body, k'cs'body) <- local (\ e@AEnv{ } -> e{ type'env = t'env' }) (infer ex'body)
        
        return (t'body, t'constrs ++ cs'body, k'constrs ++ k'cs'body)

    -- t'env <- asks type'env
    -- (t'val, cs'val, k'cs'val) <- infer ex'val
    -- case run'solve cs'val of
    --     Left err -> throwError err
    --     Right sub -> do
    --         let sc = generalize (apply sub t'env) (apply sub t'val)
    --         (t'body, cs'body, k'cs'body) <- put'in't'env (x, sc) $ local (\ e@AEnv{ type'env = t'env } -> e{ type'env = apply sub t'env }) (infer ex'body)
    --         return (t'body, cs'val ++ cs'body, k'cs'val ++ k'cs'body)

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


-- NOTE: maybe it should stay here

-- | NOTE: this can stay like this for now
infer'many :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type], [Constraint Kind])
infer'many bindings = do
  let indexed = index'bindings bindings
  let graph = build'graph bindings indexed
  let solved = stronglyConnComp graph
  -- ted to mam vyreseny a co musim udelat je
  -- ze projdu celej ten   list a pro kazdy CyclicSCC [(String, Expression)]
    -- priradim kazdymu jmenu Forall [] <$> fresh
    -- pak vlastne provedu posbirani constraintu
    -- pak je vratim nekam
  -- pro kazdy AcyclicSCC (String, Expression)
    -- tady to Expression nezavisi ani samo na sobe, takze neni potreba to zanaset
    -- jenom to infernu -> posbiram constrainty a type a vratim je nekam vejs
  infer'groups solved
    where
      infer'groups :: [SCC (String, Expression)] -> Analyze ([(String, Type)], [Constraint Type], [Constraint Kind])
      infer'groups [] = return ([], [], [])
      infer'groups ((AcyclicSCC bind) : sccs) = do
        (t'binds, constrs, k'constrs) <- infer'group [bind]
        -- (k'env, t'env, ali'env) <- ask
        t'env <- asks type'env
        (t'binds', constrs', k'constrs') <- merge'into't'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
        return (t'binds ++ t'binds', constrs ++ constrs', k'constrs ++ k'constrs')

      infer'groups ((CyclicSCC bindings) : sccs) = do
        (t'binds, constrs, k'constrs) <- infer'group bindings
        -- (k'env, t'env, ali'ev) <- ask
        t'env <- asks type'env
        (t'binds', constrs', k'constrs') <- merge'into't'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
        return (t'binds ++ t'binds', constrs ++ constrs', k'constrs ++ k'constrs')


-- | NOTE: this can stay like this for now
infer'group :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type], [Constraint Kind])
infer'group bindings = do
  let names = map fst bindings
      gener name = do ForAll [] <$> (TyVar <$> fresh)
  fresh'vars <- mapM gener names
  merge'into't'env (zip names fresh'vars) $ infer'many' bindings


-- | NOTE: this can stay like this for now
infer'many' :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type], [Constraint Kind])
infer'many' [] = do
  return ([], [], [])
infer'many' ((name, expr) : exprs) = do
  (type', constraints, k'constrs) <- infer expr

  orig'type <- lookup't'env name
  (types, constrs', k'constrs') <- infer'many' exprs
  return ((name, type') : types, (orig'type, type') : constraints ++ constrs', k'constrs ++ k'constrs')
