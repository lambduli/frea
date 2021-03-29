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


-- TODO: this will be gone!
-- infer'env :: [Declaration] -> TypeEnv -> Either TypeError TypeEnv
-- infer'env binds t'env
--   = case sequence eiths of
--       Left t'err -> Left t'err
--       Right pairs -> Right $ Env $ Map.fromList pairs

--     where
--       infer'pair :: (TypeEnv, [Either TypeError (String, Scheme)]) -> Declaration -> (TypeEnv, [Either TypeError (String, Scheme)])
--       infer'pair (t'env, eiths) (Binding name exp) = case infer'expression t'env exp of
--         Left t'err -> (t'env, Left t'err : eiths)
--         Right scheme ->
--           let Env env'map = t'env
--           in (Env $ Map.insert name scheme env'map, Right (name, scheme) : eiths)
--       infer'pair acc DataDecl{} = acc

--       eiths :: [Either TypeError (String, Scheme)]
--       (t'env', eiths) = foldl infer'pair (t'env, []) binds


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


infer'top :: TypeEnv -> [(String, Expression)] -> Either TypeError TypeEnv
infer'top environment bindings =
  case run'infer'many environment (infer'many bindings) of
    Left err -> Left err
    Right (type'bindings, constraints) -> -- ([(String, Type)], [Constraint])
        case runSolve constraints of
          Left err -> Left err
          Right subst -> do
            let (Env env) = environment
                scheme'bindings = map (second (closeOver . apply subst)) type'bindings
                env' = apply subst $ Env $ env `Map.union` Map.fromList scheme'bindings
            return env'


infer'expression :: TypeEnv -> Expression -> Either TypeError Scheme
infer'expression env expr = case runInfer env (infer expr) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty


infer'many :: [(String, Expression)] -> Infer ([(String, Type)], [Constraint])
infer'many bindings = do
  -- do env musim pridat to, ze pro kazdy jmeno bindovany v listu bindings
  -- zanesu uplne cistej ForAll [] . fresh
  -- pak muzu ten list bindingu nechat vesele infernout list constraintu
  let names = map fst bindings
      gener name = do ForAll [] <$> fresh
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

  -- (Env env) <- ask
  -- case Map.lookup name env of
  --   Nothing                 -> throwError $ UnboundVariable name -- never really happens
  --   Just (ForAll _ ty'var)  -> do
  --     (types, constrs') <- infer'many' exprs
  --     return (types ++ [(name, type')], (ty'var, type') : constraints ++ constrs')
  
  -- co si myslim: tady jsem dostal realnej type vyrazu kerej je bindnutej na name
  -- mel bych pridat dalsi constraint a to ten, zastupny type pro name (ktery uz ted musi byt v envu)
  -- se musi constraintnout na ten actual type co jsem prave dostal
  -- to by realne fakt mohlo umoznit vzajemne rekurzivni bi-directional reference


typeof :: Expression -> Either TypeError Scheme
typeof = infer'expression empty't'env
