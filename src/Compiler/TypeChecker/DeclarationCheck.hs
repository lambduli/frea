module Compiler.TypeChecker.DeclarationCheck where

import qualified Data.Map.Strict as Map
import Data.Bifunctor

import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.TypeChecker.Inference
import qualified Interpreter.Value as Val
import Interpreter.Address
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.TypeEnv

import Interpreter.Evaluate


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


-- add'constr'insts :: [ConstrDecl] -> Val.Env -> Val.Env
-- add'constr'insts [] env = env
-- add'constr'insts (ConDecl name types : cons) (Val.Env env)
--   = add'constr'insts cons (Val.Env $ Map.insert name value env)
--     where
--       value = Val.Thunk (\ _ -> force con'lam $ Val.Env Map.empty)
--       par'inds = [1 .. length types]
--       params = map (\ ind -> "p" ++ show ind ) par'inds
--       vars = map Var params
--       intro = Intro name vars
--       con'lam = foldr Lam intro params


register'constr'insts :: [ConstrDecl] -> Val.Env -> Val.Env
register'constr'insts [] env = env
register'constr'insts (ConDecl name types : cons) (Val.Env env) =
  let addr = Addr $ Map.size env
      env' = Val.Env $ Map.insert name addr env
  in register'constr'insts cons env'


register'elim'insts :: String -> [ConstrDecl] -> Val.Env -> Val.Env
register'elim'insts name constrs (Val.Env env) =
  let addr        = Addr $ Map.size env
      cons'count  = length constrs
      elim'name   = "which-" ++ name
      env'        = Val.Env $ Map.insert elim'name addr env
  in  env'


generate'constr'insts :: [ConstrDecl] -> Val.Env -> Val.Memory -> Val.Memory
generate'constr'insts [] _ mem = mem
generate'constr'insts (ConDecl name types : cons) (Val.Env env) mem =
  let addr      = env Map.! name
      par'inds  = [1 .. length types]
      params    = map (\ ind -> "p" ++ show ind ) par'inds
      vars      = map Var params
      intro     = Intro name vars
      con'lam   = foldr Lam intro params
      value     = Val.Thunk (\ env mem -> force con'lam env mem) (Val.Env Map.empty)
      mem'      = Map.insert addr value mem
  in  generate'constr'insts cons (Val.Env env) mem'


generate'elim'insts :: String -> [ConstrDecl] -> Val.Env -> Val.Memory -> Val.Memory
generate'elim'insts name constructors (Val.Env env) mem =
  let 
      cons'count  = length constructors
      elim'name   = "which-" ++ name
      addr        = env Map.! elim'name
      par'inds    = [1 .. length constructors]
      params      = map (\ ind -> "destr" ++ show ind ) par'inds
      val'var     = Var "value"
      destr'vars  = map Var params
      elim        = Elim constructors val'var destr'vars
      which'elim  = Lam "value" $ foldr Lam elim params
      value       = Val.Thunk (\ env mem -> force which'elim env mem) (Val.Env env)
      mem'        = Map.insert addr value mem
  in  mem'


-- tohle prida constructory pro data do TypeEnv - explicitne otypovane
add'constrs'types :: Type -> [ConstrDecl] -> TypeEnv -> TypeEnv
add'constrs'types _ [] t'env = t'env
add'constrs'types result't (ConDecl name types : cons) (Env t'env)
  = add'constrs'types result't cons (Env $ Map.insert name scheme t'env)
    where
      type' = foldr TyArr result't types
      scheme = ForAll [] type'


add'elim'type :: String -> [ConstrDecl] -> TypeEnv -> TypeEnv
add'elim'type name constructors (Env t'env) =
  let elim'name     = "which-" ++ name
      res           = TyVar "a"
      destr'type (ConDecl name types) = foldr TyArr res types
      destrs'types  = map destr'type constructors
      which'type    = (TyCon name) `TyArr` (foldr TyArr res destrs'types)
      scheme        = ForAll ["a"] which'type
      -- TODO: it would be much better to not create the scheme HERE
      -- it would also be much better to use already implemented functions like generalize and so
      -- TODO: once I implement higher kinded types, list of the free type variables needs to reflect that
      t'env'        = Env $ Map.insert elim'name scheme t'env
  in  t'env'


-- TODO: this function is really ugly -- it has a high priority for a refactor 
-- add'elim :: String -> [ConstrDecl] -> Val.Env -> TypeEnv -> (Val.Env, TypeEnv)
-- add'elim name constructors (Val.Env env) (Env t'env) =
--   let
--     cons'count = length constructors
--     elim'name = "which-" ++ name

--     par'inds = [1 .. length constructors]
--     params = map (\ ind -> "destr" ++ show ind ) par'inds
--     val'var = Var "value"
--     destr'vars = map Var params
--     elim = Elim constructors val'var destr'vars
--     which'elim = Lam "value" $ foldr Lam elim params
--     value = Val.Thunk (\ _ -> force which'elim $ Val.Env env)
--     env' = Val.Env $ Map.insert elim'name value env
--     -- again - I am closing the which-elim in the environment, which doesn't contain the which-elim
--     -- itself --> it won't be able to call it inside I am afraid
--     -- I will have to fix that!!!

--     res = TyVar "a"
--     destr'type (ConDecl name types) = foldr TyArr res types
--     destrs'types = map destr'type constructors
--     which'type = (TyCon name) `TyArr` (foldr TyArr res destrs'types)
--     scheme = ForAll ["a"] which'type
--     -- TODO: it would be much better to not create the scheme HERE
--     -- it would also be much better to use already implemented functions like generalize and so
--     -- TODO: once I implement higher kinded types, list of the free type variables needs to reflect that
--     t'env' = Env $ Map.insert elim'name scheme t'env
--   in (env', t'env')


process'declarations :: [Declaration] -> Val.Env -> TypeEnv -> Val.Memory -> [Type] -> Either String (Val.Env, TypeEnv, [Type], Val.Memory)
process'declarations declarations env t'env mem type'ctx = do
  -- ze vseho nejdriv type checknu data deklarace
  let type'ctx' = foldl collect'types type'ctx declarations
  type'check'data'types type'ctx declarations
  
  -- nejdriv musim vyrobit Env
  -- ten obsahuje jenom identifikatory a adresy
  let env' = foldl register'declarations (Val.Env Map.empty) declarations
  -- ted mam kompletni Env a ten obsahuje bindingy pro vsechny identifikatory
  -- ted muzu timhle envem closovat libovolny Values a pokud pri evaluaci bude v memory
  -- na spravne adrese implementace, bude to v poradku

  -- ted musim projit vsechny deklarace znova a tentokrat skutecne vyrobit Values
  -- a pri tom foldovani musim vyrobit i Memory
  let mem = foldl (construct'declarations env') Map.empty declarations


  let t'env' = foldl add'types t'env declarations
  
  
  
  -- res <-
  --   foldl
  --     close'with
  --     (Right (env, t'env, type'ctx))
  --     declarations
  -- let (env', t'env', type'ctx) = res
  -- let env'' = Val.Env $ Map.map (second (const $ Val.Env env')) env'
  -- NOTE2: I had to comment it out - now when the Env contains Values it's not really easy to re-close them you know
  -- NOTE: this doesn't really help anything that much
  -- so don't worry about removing it later
  -- or fixing it, for that matter

  return (env', t'env', type'ctx, mem)

    where
      collect'types :: [Type] -> Declaration -> [Type]
      collect'types type'ctx decl =
        case decl of
          DataDecl name _ constrs ->
            TyCon name : type'ctx

          _ -> type'ctx


      type'check'data'types :: [Type] -> [Declaration] -> Either String ()
      type'check'data'types type'ctx [] = Right ()
      type'check'data'types type'ctx (decl : decls) =
        case decl of
          DataDecl name _ constrs ->
            check'constrs constrs type'ctx
          _ -> type'check'data'types type'ctx decls


      register'declarations :: Val.Env -> Declaration -> Val.Env
      register'declarations env@(Val.Env e'map) decl =
        case decl of
          Binding name expr ->
            let addr = Addr $ Map.size e'map
            in Val.Env $ Map.insert name addr e'map

          DataDecl name _ constrs ->
            let env'  = register'constr'insts constrs env
                env'' = register'elim'insts name constrs env'
            in  env''

          _ -> (Val.Env e'map)


      construct'declarations :: Val.Env -> Val.Memory -> Declaration -> Val.Memory
      construct'declarations env@(Val.Env e'map) mem decl =
        case decl of
          Binding name expr ->
            let addr = e'map Map.! name
                val  = Val.Thunk (\ env mem -> force expr env mem) env
                mem' = Map.insert addr val mem
            in  mem'

          DataDecl name _ constrs ->
            -- [ConstrDecl] -> Val.Env -> Val.Memory -> Val.Memory
            let mem'  = generate'constr'insts constrs env mem
                mem'' = generate'elim'insts name constrs env mem'
            in  mem''

          _ -> mem


      add'types :: TypeEnv -> Declaration -> TypeEnv
      add'types t'env decl =
        case decl of
          DataDecl name _ constrs ->
            let t'env'  = add'constrs'types (TyCon name) constrs t'env
                t'env'' = add'elim'type name constrs t'env'
            in  t'env''
          _ -> t'env


      -- close'with :: Either String (Val.Env, TypeEnv, [Type]) -> Declaration -> Either String (Val.Env, TypeEnv, [Type])
      -- close'with (Right (env, t'env, type'ctx)) declaration =
      --   case declaration of
      --     -- Binding name expr ->
      --     --   let
      --     --     Val.Env env'map = env
      --     --     value = Val.Thunk (\ (env, mem) -> force expr env mem) (env, mem)
      --     --     new'env = Val.Env $ Map.insert name value env'map
      --     --   in
      --     --     Right (new'env, t'env, type'ctx)

      --     DataDecl name _ constrs ->
      --       -- TODO: type checkovani constructoru udelam jinde, zatim to teda neni treba resit
      --       -- urcite ne tady
      --       case check'constrs constrs (TyCon name : type'ctx) of
      --         -- Left err ->
      --           -- Left err
      --         Right _ ->
      --           let
      --             t'env' = add'constrs (TyCon name) constrs t'env
      --             -- TODO: stejne tak bych do TypeEnvu mel pridat typy constructoru uplne jinde
      --             -- tady to neni starost

      --             env' = add'constr'insts constrs env
      --             (env'', t'env'') = add'elim name constrs env' t'env'
      --           in Right (env'', t'env'', TyCon name : type'ctx)
      -- close'with (Left err) _ = Left err
