module Compiler.TypeChecker.DeclarationCheck where

import qualified Data.Map.Strict as Map
import Data.Bifunctor

import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.TypeChecker.Inference
import qualified Interpreter.Value as Val
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.TypeEnv


check'constrs :: [ConstrDecl] -> [Type] -> Either String ()
check'constrs [] _ = Right ()
check'constrs (ConDecl name types : cons) type'ctx = do
  check types
    where
      check :: [Type] -> Either String ()
      check [] = Right ()
      check (t : ts) =
        case t of
          TyVar _ -> Right ()
          TyCon n
            | t `elem` type'ctx -> Right ()
            | otherwise -> Left $ "Type error: Unknown type constructor " ++ n ++ "." 
          TyTuple types -> do
            check types
          TyList t -> do
            check [t]
          TyArr from't to't -> do
            check [from't]
            check [to't]


add'constrs :: Type -> [ConstrDecl] -> TypeEnv -> TypeEnv
add'constrs _ [] t'env = t'env
add'constrs result't (ConDecl name types : cons) (Env t'env)
  = add'constrs result't cons (Env $ Map.insert name scheme t'env)
    where
      type' = foldr TyArr result't types
      scheme = ForAll [] type'


add'constr'insts :: [ConstrDecl] -> Val.Env -> Val.Env
add'constr'insts [] env = env
add'constr'insts (ConDecl name types : cons) (Val.Env env)
  = add'constr'insts cons (Val.Env $ Map.insert name (con'lam, Val.Env Map.empty) env)
    where
      par'inds = [1 .. length types]
      params = map (\ ind -> "p" ++ show ind ) par'inds
      vars = map Var params
      intro = Intro name vars
      con'lam = foldr Lam intro params


-- TODO: this function is really ugly -- it has a high priority for a refactor 
add'elim :: String -> [ConstrDecl] -> Val.Env -> TypeEnv -> (Val.Env, TypeEnv)
add'elim name constructors (Val.Env env) (Env t'env) =
  let
    cons'count = length constructors
    elim'name = "which-" ++ name

    par'inds = [1 .. length constructors]
    params = map (\ ind -> "destr" ++ show ind ) par'inds
    val'var = Var "value"
    destr'vars = map Var params
    elim = Elim constructors val'var destr'vars
    which'elim = foldr Lam elim params
    env' = Val.Env $ Map.insert elim'name (which'elim, Val.Env env) env
    -- again - I am closing the which-elim in the environment, which doesn't contain the which-elim
    -- itself --> it won't be able to call it inside I am afraid
    -- I will have to fix that!!!

    res = TyVar "a"
    destr'type (ConDecl name types) = foldr TyArr res types
    destrs'types = map destr'type constructors
    which'type = (TyCon name) `TyArr` (foldr TyArr res destrs'types)
    scheme = ForAll ["a"] which'type
    -- TODO: it would be much better to not create the scheme HERE
    -- it would also be much better to use already implemented functions like generalize and so
    -- TODO: once I implement higher kinded types, list of the free type variables needs to reflect that
    t'env' = Env $ Map.insert elim'name scheme t'env
  in (env', t'env')


process'declarations :: [Declaration] -> Val.Env -> TypeEnv -> [Type] -> Either String (Val.Env, TypeEnv, [Type])
process'declarations declarations env t'env type'ctx = do
  res <-
    foldl
      close'with
      (Right (env, t'env, type'ctx))
      declarations
  let (Val.Env env', t'env', type'ctx) = res
  let env'' = Val.Env $ Map.map (second (const $ Val.Env env')) env'
  -- NOTE: this doesn't really help anything that much
  -- so don't worry about removing it later
  -- or fixing it, for that matter

  return (env'', t'env', type'ctx)

    where
      close'with :: Either String (Val.Env, TypeEnv, [Type]) -> Declaration -> Either String (Val.Env, TypeEnv, [Type])
      close'with (Right (env, t'env, type'ctx)) declaration =
        case declaration of
          Binding name expr ->
            let
              Val.Env env'map = env
              new'env = Val.Env $ Map.insert name (expr, env) env'map
            in
              Right (new'env, t'env, type'ctx)

          DataDecl name _ constrs ->
            case check'constrs constrs (TyCon name : type'ctx) of
              Left err ->
                Left err
              Right _ ->
                let
                  t'env' = add'constrs (TyCon name) constrs t'env
                  env' = add'constr'insts constrs env
                  (env'', t'env'') = add'elim name constrs env' t'env'
                in Right (env'', t'env'', TyCon name : type'ctx)
      close'with (Left err) _ = Left err
