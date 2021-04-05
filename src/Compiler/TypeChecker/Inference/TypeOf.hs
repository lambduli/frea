module Compiler.TypeChecker.Inference.TypeOf where


import qualified Data.Map.Strict as Map
import Data.Bifunctor
import Control.Monad

import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.Syntax.Expression
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Inference
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.Solver
import Compiler.TypeChecker.Inference.Substituable
import Compiler.TypeChecker.Inference.Constraint
import Compiler.TypeChecker.Inference.TypeEnv
import Compiler.TypeChecker.Inference.InferUtils

import Compiler.KindChecker.KindEnv
import Compiler.KindChecker.Inference hiding (infer)
import Compiler.KindChecker.KindError


infer'env :: [Declaration] -> TypeEnv -> Either TypeError TypeEnv
infer'env binds t'env = do
  let only'funs = filter is'fun binds
      pairs = map to'pair only'funs
  infer'top t'env pairs
    where
      is'fun :: Declaration -> Bool
      is'fun (Binding _ _) = True
      is'fun _ = False

      to'pair :: Declaration -> (String, Expression)
      to'pair (Binding name expr) = (name, expr)


infer'decls :: [Declaration]  -> KindEnv -> Either KindError KindEnv
infer'decls binds k'env = do
  let only'data = filter is'data binds
      data'pairs = map to'pair only'data

  infer'data k'env data'pairs
  
    where
      is'data :: Declaration -> Bool
      is'data (DataDecl _ _ _) = True
      is'data _ = False

      to'pair :: Declaration -> (String, Declaration)
      to'pair d@(DataDecl name _ _) = (name, d)


infer'top :: TypeEnv -> [(String, Expression)] -> Either TypeError TypeEnv
infer'top environment bindings =
  case run'infer'many environment (infer'many bindings) of
    Left err -> Left err
    Right (type'bindings, constraints) -> -- ([(String, Type)], [Constraint])
        case runSolve constraints of
          Left err -> Left err
          Right subst -> do
            let scheme'bindings = map (second (closeOver . apply subst)) type'bindings
                env' = apply subst $ environment `Map.union` Map.fromList scheme'bindings
            return env'


infer'expression :: TypeEnv -> Expression -> Either TypeError Scheme
infer'expression env expr = case runInfer env (infer expr) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty


infer'many :: [(String, Expression)] -> Infer ([(String, Type)], [Constraint])
infer'many bindings = do
  let names = map fst bindings
      gener name = do
        var <- fresh
        let (TyVar var'name) = var
        return $ ForAll [var'name] (TyVar var'name)
  fresh'vars <- mapM gener names
  merge'into'env (zip names fresh'vars) $ infer'many' bindings
  

infer'many' :: [(String, Expression)] -> Infer ([(String, Type)], [Constraint])
infer'many' [] = do
  return ([], [])
infer'many' ((name, expr) : exprs) = do
  (type', constraints) <- infer expr

  orig'type <- lookup'env name
  (types, constrs') <- infer'many' exprs
  return ((name, type') : types, (orig'type, type') : constraints ++ constrs')
  
  -- this should actually work
  -- instantiate should do nothing to the fresh type variable because the ForAll
  -- has an empty list of type parameters

typeof :: Expression -> Either TypeError Scheme
typeof = infer'expression empty't'env
