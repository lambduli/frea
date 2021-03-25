module Compiler.TypeChecker.Inference where

import qualified Data.Map.Strict as Map
import Data.List (elem, foldl)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Bifunctor (second)
import Data.Functor.Identity

import Compiler.TypeChecker.Type 
import Compiler.Syntax.Declaration
import Compiler.TypeChecker.Inference.Utils
import Compiler.Syntax
  ( Bind(..)
  , Declaration(..), ConstrDecl(..)
  , Expression(..)
  , Lit(..)
  , MatchGroup(..), Match(..)
  , Pattern(..)
  , Sig(..)
  , Type(..), Scheme(..))
import Compiler.Syntax.Expression
import Compiler.TypeChecker.TypeError


put'in'env :: (String, Scheme) -> Infer a -> Infer a
put'in'env (var, scheme) m = do
  let scope e = remove e var `extend` (var, scheme)
  local scope m


lookup'env :: String -> Infer Type
lookup'env var = do
  (Env env) <- ask
  case Map.lookup var env of
    Nothing   ->  throwError $ UnboundVariable var
    Just s    ->  instantiate s


-- Return the internal constraints used in solving for the type of an expression
-- for debugging ?
constraintsExpr :: TypeEnv -> Expression -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env expr = case runInfer env (infer expr) of
  Left err -> Left err
  Right (type', constraints) -> case runSolve constraints of
    Left err -> Left err
    Right subst -> Right (constraints, subst, type', scheme)
      where
        scheme = closeOver $ apply subst type'


infer :: Expression -> Infer (Type, [Constraint])
infer expr = case expr of  
  Lit (LitInt i) -> return (t'Int, [])
  Lit (LitDouble d) -> return (t'Double, [])
  Lit (LitChar ch) -> return (t'Char, [])
  Lit (LitString s) -> return (TyList t'Char, [])
  Lit (LitBool b) -> return (t'Bool, [])
  Lit LitUnit -> return (t'Unit, [])

  (Var x) -> do
    type' <- lookup'env x
    return (type', [])

  Op x -> do
    type' <- lookup'env x
    return (type', [])

  Lam x body -> do
    t'var <- fresh
    (t, constrs) <- put'in'env (x, ForAll [] t'var) (infer body)
    return (t'var `TyArr` t, constrs)

  App left right -> do
    (t'l, cs'l) <- infer left
    (t'r, cs'r) <- infer right
    t'var <- fresh
    return (t'var, cs'l ++ cs'r ++ [(t'l, t'r `TyArr` t'var)])

  If cond tr fl -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    return (t2, c1 ++ c2 ++ c3 ++ [(t1, t'Bool), (t2, t3)])
  
  Let x ex'val ex'body -> do
    env <- ask
    (t'val, cs'val) <- infer ex'val
    case runSolve cs'val of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (apply sub env) (apply sub t'val)
            (t'body, cs'body) <- put'in'env (x, sc) $ local (apply sub) (infer ex'body)
            return (t'body, cs'val ++ cs'body)

  Fix expr -> do
    (type', cs) <- infer expr
    t'var <- fresh
    return (t'var, cs ++ [(t'var `TyArr` t'var, type')])

  Tuple exprs -> do
    (types, cs) <- foldM infer' ([], []) exprs
    return (TyTuple $ reverse types, cs)
      where
        infer' (types, constrs) expr = do
          (t, cs) <- infer expr
          return (t : types, cs ++ constrs)

  List exprs -> do
    type'var <- fresh
    let infer' costrs expr = do
          (t, cs) <- infer expr
          return $ (type'var, t) : cs
    constrs <- foldM infer' [] exprs
    return (TyList type'var, constrs)
