module Compiler.TypeChecker.DeclarationCheck where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Bifunctor

import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.TypeChecker.Inference
import qualified Interpreter.Value as Val
import Interpreter.Address
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.TypeEnv
import Compiler.TypeChecker.Inference.Substituable

import Interpreter.Evaluate

import Compiler.KindChecker.KindEnv
import Compiler.TypeChecker.Inference.TypeOf


register'constr'insts :: [ConstrDecl] -> Val.Env -> Val.Env
register'constr'insts [] env = env
register'constr'insts (ConDecl name types : cons) env =
  let addr = Addr $ Map.size env
      env' = Map.insert name addr env
  in register'constr'insts cons env'


register'elim'insts :: String -> [ConstrDecl] -> Val.Env -> Val.Env
register'elim'insts name constrs env =
  let addr        = Addr $ Map.size env
      cons'count  = length constrs
      elim'name   = "which-" ++ name
      env'        = Map.insert elim'name addr env
  in  env'


generate'constr'insts :: [ConstrDecl] -> Val.Env -> Val.Memory -> Val.Memory
generate'constr'insts [] _ mem = mem
generate'constr'insts (ConDecl name types : cons) env mem =
  let addr      = env Map.! name
      par'inds  = [1 .. length types]
      params    = map (\ ind -> "p" ++ show ind ) par'inds
      vars      = map Var params
      intro     = Intro name vars
      con'lam   = foldr Lam intro params
      value     = Val.Thunk (\ env -> force con'lam env) Val.empty'env -- I don't need anything from the Env
      mem'      = Map.insert addr (Val.At addr value) mem
  in  generate'constr'insts cons env mem'


generate'elim'insts :: String -> [ConstrDecl] -> Val.Env -> Val.Memory -> Val.Memory
generate'elim'insts name constructors env mem =
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
      value       = Val.Thunk (\ env -> force which'elim env) env
      mem'        = Map.insert addr (Val.At addr value) mem
  in  mem'


-- tohle prida constructory pro data do TypeEnv - explicitne otypovane
add'constrs'types :: Type -> [ConstrDecl] -> TypeEnv -> TypeEnv
add'constrs'types _ [] t'env = t'env
add'constrs'types result't (ConDecl name types : cons) t'env
  = add'constrs'types result't cons (Map.insert name scheme t'env)
    where
      type' = foldr TyArr result't types
      ty'params = Set.toList $ ftv type'
      scheme = ForAll ty'params type'


add'elim'type :: String -> Type -> [ConstrDecl] -> TypeEnv -> TypeEnv
add'elim'type name result't constructors t'env =
  let elim'name     = "which-" ++ name
      res           = TyVar "@:z" -- TODO: this needs to be fresh variable!!! -- for now making it somehow hard to mix up with anything
      destr'type (ConDecl name types) = foldr TyArr res types
      destrs'types  = map destr'type constructors
      which'type    = result't `TyArr` (foldr TyArr res destrs'types)
      scheme        = ForAll (Set.toList $ ftv which'type) which'type
      -- TODO: it would be much better to not create the scheme HERE
      -- it would also be much better to use already implemented functions like generalize and so
      -- TODO: once I implement higher kinded types, list of the free type variables needs to reflect that
      t'env'        = Map.insert elim'name scheme t'env
  in  t'env'


process'declarations :: [Declaration] -> Val.Env -> TypeEnv -> KindEnv -> Val.Memory -> Either String (Val.Env, TypeEnv, KindEnv, Val.Memory)
process'declarations declarations env t'env k'env mem = do
  k'env' <- case infer'decls declarations k'env of
    Left k'err -> Left $ show k'err
    Right k'env' -> Right k'env'

  -- nejdriv musim vyrobit Env
  -- ten obsahuje jenom identifikatory a adresy
  let env' = foldl register'declarations env declarations
  -- ted mam kompletni Env a ten obsahuje bindingy pro vsechny identifikatory
  -- ted muzu timhle envem closovat libovolny Values a pokud pri evaluaci bude v memory
  -- na spravne adrese implementace, bude to v poradku

  -- ted musim projit vsechny deklarace znova a tentokrat skutecne vyrobit Values
  -- a pri tom foldovani musim vyrobit i Memory
  let mem' = foldl (construct'declarations env') mem declarations
  let t'env' = foldl add'types t'env declarations

  return (env', t'env', k'env', mem')

    where
      register'declarations :: Val.Env -> Declaration -> Val.Env
      register'declarations env decl =
        case decl of
          Binding name expr ->
            let addr = Addr $ Map.size env
            in Map.insert name addr env

          DataDecl name _ constrs ->
            let env'  = register'constr'insts constrs env
                env'' = register'elim'insts name constrs env'
            in  env''

          _ -> env


      construct'declarations :: Val.Env -> Val.Memory -> Declaration -> Val.Memory
      construct'declarations env mem decl =
        case decl of
          Binding name expr ->
            let addr = env Map.! name
                val  = Val.Thunk (\ env -> force expr env) env
                mem' = Map.insert addr (Val.At addr val) mem
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
          DataDecl name ty'params constrs ->
            let res'type = foldl (\ t var -> TyApp t (TyVar var)) (TyCon name) ty'params
                t'env'  = add'constrs'types res'type constrs t'env
                t'env'' = add'elim'type name res'type constrs t'env'
            in  t'env''
          _ -> t'env
