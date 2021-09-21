{-# LANGUAGE TupleSections #-}

module Compiler.TypeAnalyzer.TypeOf where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Map.Strict ((!), (!?))
import Data.List (partition)
import Data.Bifunctor

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.State


import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.Syntax.Expression

import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.Solver
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.Constraint
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.AnalyzeUtils
import Compiler.TypeAnalyzer.Dependency
import qualified Compiler.TypeAnalyzer.Type.Evaluate as E

import Compiler.TypeAnalyzer.Type.Analyze
import Compiler.TypeAnalyzer.Kind.KindOf

-- import Compiler.TypeAnalyzer.Inference.Many

import Interpreter.Value (Env, Memory)
import qualified Interpreter.Value as Val
import Interpreter.Address
import Interpreter.Evaluate


--
--
--



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
      value     = Val.Thunk (\ env -> force con'lam env) Val.empty'env addr -- I don't need anything from the Env
      mem'      = Map.insert addr value mem
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
      value       = Val.Thunk (\ env -> force which'elim env) env addr
      mem'        = Map.insert addr value mem
  in  mem'


-- tohle prida constructory pro data do TypeEnv - explicitne otypovane
add'constrs'types :: Type -> [ConstrDecl] -> TypeEnv -> TypeEnv
add'constrs'types _ [] t'env = t'env
add'constrs'types result't (ConDecl name types : cons) t'env
  = add'constrs'types result't cons (Map.insert name scheme t'env)
    where
      type' = foldr TyArr result't types
      ty'params = Set.toList $ free'vars type'
      scheme = ForAll ty'params type'


add'elim'type :: String -> Type -> [ConstrDecl] -> TypeEnv -> Analyze TypeEnv
add'elim'type name result't constructors t'env = do
  fresh'name <- fresh
  let elim'name     = "which-" ++ name
      res           = TyVar fresh'name -- TODO: this needs to be fresh variable!!! -- for now making it somehow hard to mix up with anything
      destr'type (ConDecl name types) = foldr TyArr res types
      destrs'types  = map destr'type constructors
      which'type    = result't `TyArr` (foldr TyArr res destrs'types)
      scheme        = generalize empty't'env which'type
        
        -- ForAll (Set.toList $ free'vars which'type) which'type
      -- TODO: it would be much better to not create the scheme HERE
      -- it would also be much better to use already implemented functions like generalize and so
      -- TODO: once I implement higher kinded types, list of the free type variables needs to reflect that
      t'env'        = Map.insert elim'name scheme t'env
  return t'env'


process'declarations :: [Declaration] -> Val.Env -> TypeEnv -> Val.Memory -> Analyze (Val.Env, TypeEnv, Val.Memory)
process'declarations declarations env t'env mem = do
  -- k'env' <- case infer'decls declarations k'env of
  --   Left k'err -> Left $ show k'err
  --   Right k'env' -> Right k'env'

  -- nejdriv musim vyrobit Env
  -- ten obsahuje jenom identifikatory a adresy
  let env' = foldl register'declarations env declarations
  -- ted mam kompletni Env a ten obsahuje bindingy pro vsechny identifikatory
  -- ted muzu timhle envem closovat libovolny Values a pokud pri evaluaci bude v memory
  -- na spravne adrese implementace, bude to v poradku

  -- ted musim projit vsechny deklarace znova a tentokrat skutecne vyrobit Values
  -- a pri tom foldovani musim vyrobit i Memory
  let mem' = foldl (construct'declarations env') mem declarations
  t'env' <- foldM add'types t'env declarations

  return (env', t'env', mem')

    where
      register'declarations :: Val.Env -> Declaration -> Val.Env
      register'declarations env decl =
        case decl of
          Binding name expr ->
            let addr = Addr $ Map.size env
            in Map.insert name addr env

          Annotated name type' expr ->
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
            let addr  = env Map.! name
                val   = Val.Thunk (\ env -> force expr env) env addr
                mem'  = Map.insert addr val mem
            in  mem'

          Annotated name type' expr ->
            let addr  = env Map.! name
                val   = Val.Thunk (\ env -> force expr env) env addr
                mem'  = Map.insert addr val mem
            in mem'

          DataDecl name _ constrs ->
            -- [ConstrDecl] -> Val.Env -> Val.Memory -> Val.Memory
            let mem'  = generate'constr'insts constrs env mem
                mem'' = generate'elim'insts name constrs env mem'
            in  mem''

          _ -> mem


      add'types :: TypeEnv -> Declaration -> Analyze TypeEnv
      add'types t'env decl =
        case decl of
          DataDecl name ty'params constrs -> do
            let res'type = foldl (\ t var -> TyApp t (TyVar var)) (TyCon name) ty'params
                t'env'  = add'constrs'types res'type constrs t'env
            add'elim'type name res'type constrs t'env'

          _ -> return t'env


--
--
--


-- TODO: musim vzit ali'env a expandovat vsechny typovy anotace v tom expr
-- idealne bych jeste taky ty anotace vzal a posbiral z nich constrainty uz tak jak jsou?
infer'expression :: Expression -> Analyze Scheme
infer'expression expr = do
  ex'expr <- expand'expr expr
  (type', constraints, k'constrs) <- infer ex'expr -- TODO: it would be better to also solve the kind constraints
  case run'solve constraints  of
      Left err -> throwError err
      Right subst ->
        case run'solve k'constrs of -- Just also check if kinds are correct
          Left err -> throwError err
          Right _ -> do
            return $ closeOver $ apply subst type'


{-
Potrebuju napsat funkci, ktera vezme vsechny deklarace a type'env, kind'env, memory, env a ali'env

pak musim provest vsechno z tohohle:

1 Projit deklarace `data` a vygenerovat pro ne lambda constructory, pridat typy a podobne
zkratka jde o rezii, ktera se ted dela v modulu DeclarationCheck.

2 Projit deklarace `type` synonyms a zanest je do ali'env + evaluovat vsechny typy
znormalizovat typovy termy tak, aby neobsahovali synonyma ale jejich obsah

Tady je na miste otazka - nemel bych vygenerovat constrainty i ve stavu, kde jeste synonyma jsou,
pak je teprve vyhodnotit, znova vygenerovat dalsi synonyma spojit dohromady a pak teprve resit?

3 Projit deklarace constant a funkci. Provest type inferenci a type checking.

-}


-- TODO: add the implementation from the DeclarationCheck module
analyze'module :: [Declaration] -> (Env, Memory) -> Analyze (AnalyzeEnv, Env, Memory)
analyze'module decls (env, mem) = do
  -- (k'env, t'env, ali'env, env, mem)

  AEnv{ kind'env = k'e, type'env = t'e, ali'env = a'e } <- ask

  (env', t'e', mem') <- process'declarations decls env t'e mem
  let ali'env = Map.union a'e $ make'alias'env only'aliases

  local (const (AEnv k'e t'e' ali'env)) (analyze' env' mem')

    where
      only'aliases  = filter is'alias decls
      only'funs     = filter is'fun decls
      only'types    = filter is'type decls

      analyze' :: Val.Env -> Memory -> Analyze (AnalyzeEnv, Env, Memory)
      analyze' env mem = do
        check'for'synonym'cycles only'aliases
        -- TODO: maybe collect some constraints from unevaluated type annotations?
        expanded'funs <- mapM expand'aliases only'funs
        let fun'pairs = map name'expr expanded'funs

        -- TODO: maybe collect some constraints from unevaluated type declarations?
        expanded'types <- mapM expand'aliases only'types
        let type'pairs = map lift'name expanded'types

        -- to co musim udelat je ekvivalentni infer'top + infer'data

        -- hodim si sem in place analyze'type'decsl
        -- analyze'type'decls bindings k'constrs = do
        (kind'bindings, k'constrs) <- analyze'types type'pairs
        -- sesbiram kind constrainty
        
        (t'env, k'constrs') <- local (\ e@AEnv{ kind'env = k'e } -> e{ kind'env = k'e `Map.union` Map.fromList kind'bindings }) $ analyze'top'decls fun'pairs
        
        k'env <- case run'solve (k'constrs ++ k'constrs') of
          Left err -> throwError err
          Right subst -> do
            k'env <- asks kind'env
            return $ specify'k'vars $ apply subst $ k'env `Map.union` Map.fromList kind'bindings

        ali'env <- asks ali'env

        return (AEnv k'env t'env ali'env, env, mem)


      name'expr :: Declaration -> (String, Expression)
      name'expr (Binding name expr) = (name, expr)
      name'expr (Annotated name type' expr) = (name, Ann type' expr)

      lift'name :: Declaration -> (String, Declaration)
      lift'name d@(TypeAlias name _) = (name, d)
      lift'name d@(DataDecl name _ _) = (name, d)

      make'alias'env :: [Declaration] -> AliEnv
      make'alias'env decls
        = Map.fromList $ map (\ (TypeAlias name type') -> (name, type')) decls

      is'fun, is'alias, is'type :: Declaration -> Bool

      is'fun (Binding _ _) = True
      is'fun (Annotated _ _ _) = True
      is'fun _ = False

      is'alias (TypeAlias _ _) = True
      is'alias _ = False

      is'type (DataDecl _ _ _) = True
      is'type (TypeAlias _ _) = True
      is'type _ = False

      expand'aliases :: Declaration -> Analyze Declaration
      expand'aliases (Binding name expr) = do
        ex'expr <- expand'expr expr
        return $ Binding name ex'expr
      expand'aliases a@(Annotated name type' expr) = do
        norm'type' <- E.evaluate type'
        expanded'expr <- expand'expr expr
        return $ Annotated name norm'type' expanded'expr
      expand'aliases (DataDecl name params constructors) = do
        constrs <- mapM expand'constr constructors
        return $ DataDecl name params constrs
      expand'aliases (TypeAlias name type') = do
        n'type <- E.evaluate type'
        return $ TypeAlias name n'type
      expand'aliases impossible = return impossible

      expand'constr :: ConstrDecl -> Analyze ConstrDecl
      expand'constr (ConDecl name param'types) = do
        par't <- mapM E.evaluate param'types
        return $ ConDecl name par't


-- | This functions is the counterpart of the analyze'type'decls
analyze'top'decls :: [(String, Expression)] -> Analyze (TypeEnv, [Constraint Kind])
analyze'top'decls fun'pairs = do
  (t'env', t'constrs, k'constrs) <- infer'definitions fun'pairs
  return (t'env', k'constrs)

  -- (type'bindings, t'constrs, k'constrs) <- infer'many fun'pairs
  -- case run'solve t'constrs  of
  --   Left err -> throwError err
  --   Right subst -> do
  --     t'env <- asks type'env
  --     let scheme'bindings = map (second (closeOver . apply subst)) type'bindings
  --         env' = apply subst $ t'env `Map.union` Map.fromList scheme'bindings
  --     return (env', k'constrs)


{-  This function finds all type annotations in the given Expression
    and evaluates them.
    UPDATED
-}
expand'expr :: Expression -> Analyze Expression
expand'expr expr =
  case expr of
    Var name -> return $ Var name
    Op name -> return $ Op name
    Lit lit -> return $ Lit lit
    Lam par body -> do
      ex'body <- expand'expr body
      return $ Lam par ex'body
    App left right -> do
      ex'left <- expand'expr left
      ex'right <- expand'expr right
      return $ App ex'left ex'right
    Tuple exprs -> do
      ex'exprs <- mapM expand'expr exprs
      return $ Tuple ex'exprs
    If cond' then' else' -> do
      ex'cond <- expand'expr cond'
      ex'then <- expand'expr then'
      ex'else <- expand'expr else'
      return $ If ex'cond ex'then ex'else
    Let bind'pairs expr -> do
      -- NOTE: check it later
      let ex'b'p = map (second expand'expr) bind'pairs
      ex'bind'pairs <- mapM (\ (n, v) -> (n, ) <$> expand'expr v) bind'pairs
      -- ex'val <- expand'expr value
      ex'expr <- expand'expr expr
      return $ Let ex'bind'pairs ex'expr
    Ann type' expr -> do
      norm'type <- E.evaluate type'
      ex'expr <- expand'expr expr
      return $ Ann norm'type ex'expr
    Intro name exprs ->
      return $ Intro name exprs -- NOTE: I can safely do that - this is generated, no type annotations can get there
    Elim cons'decls val destrs ->
      return $ Elim cons'decls val destrs -- NOTE: I can safely do that - this is generated, no type annotations can get there
