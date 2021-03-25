module Compiler.TypeChecker.DeclarationCheck where

import qualified Data.Map.Strict as Map

import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.TypeChecker.Inference
import qualified Interpreter.Value as Val
import Compiler.TypeChecker.Inference.Utils


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


process'declarations :: [Declaration] -> Val.Env -> TypeEnv -> [Type] -> Either String (Val.Env, TypeEnv, [Type])
process'declarations declarations env t'env type'ctx =
  let
    res = foldl
      close'with
      (Right (env, t'env, [], type'ctx))
      declarations
  in
    case res of
      Left err -> Left err
      Right (env', t'env', closed, type'ctx') ->
        Right (env', t'env', type'ctx')

    where
      close'with :: Either String (Val.Env, TypeEnv, [(String, Val.Closed)], [Type]) -> Declaration -> Either String (Val.Env, TypeEnv, [(String, Val.Closed)], [Type])
      close'with (Right (env, t'env, binds, type'ctx)) declaration =
        case declaration of
          Binding name expr ->
            let
              Val.Env env'map = env
              new'env = Val.Env $ Map.insert name (expr, env) env'map
              closed = (expr, new'env)
            in
              Right (new'env, t'env, (name, closed) : binds, type'ctx)

          DataDecl name _ constrs ->
            case check'constrs constrs (TyCon name : type'ctx) of
              Left err ->
                Left err
              Right _ ->
                let
                  t'env' = add'constrs (TyCon name) constrs t'env
                  env' = add'constr'insts constrs env
                in Right (env', t'env', binds, TyCon name : type'ctx)
      close'with (Left err) _ = Left err
