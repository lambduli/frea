module Compiler.TypeChecker.TypeOf where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Map.Strict ((!), (!?))
import Data.List (partition)
import Data.Bifunctor

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State


import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.Syntax.Expression

import Compiler.TypeChecker.Error
import Compiler.TypeChecker.Analyze
import Compiler.TypeChecker.Solver
import Compiler.TypeChecker.Substituable
import Compiler.TypeChecker.Constraint
import Compiler.TypeChecker.AnalyzeEnv
import Compiler.TypeChecker.AnalyzeUtils
import Compiler.TypeChecker.Dependency
import qualified Compiler.TypeChecker.Type.Evaluate as E

import Compiler.TypeChecker.Type.Analyze
import Compiler.TypeChecker.Kind.KindOf

import Interpreter.Value (Env, Memory)


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
analyze'module :: [Declaration] -> Analyze (KindEnv, TypeEnv, AliEnv, Env, Memory)
analyze'module decls = do
  -- (k'env, t'env, ali'env, env, mem)
  local (\ (k, t, a) -> (k, t, ali'env a)) analyze'

    where
      only'aliases  = filter is'alias decls
      only'funs     = filter is'fun decls
      only'types    = filter is'type decls
      ali'env a     = Map.union a $ make'alias'env only'aliases -- TODO: this is awkward, pls fix

      analyze' :: Analyze (KindEnv, TypeEnv, AliEnv, Env, Memory)
      analyze' = do
        check'for'synonym'cycles only'aliases
        -- TODO: maybe collect some constraints from unevaluated type annotations?
        expanded'funs <- mapM expand'aliases only'funs
        let fun'pairs = map name'expr expanded'funs

        -- TODO: maybe collect some constraints from unevaluated type declarations?
        expanded'types <- mapM expand'aliases only'types
        let type'pairs = map lift'name expanded'types

        -- to co musim udelat je ekvivalentni infer'top + infer'data

        t'env <- analyze'top'decls fun'pairs
        
        k'env <- analyze'type'decls type'pairs


        return undefined


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
analyze'top'decls :: [(String, Expression)] -> Analyze TypeEnv
analyze'top'decls fun'pairs = do
  (type'bindings, t'constrs) <- infer'many fun'pairs
  case runSolve t'constrs  of
    Left err -> throwError err
    Right subst -> do
      (_, t'env, _) <- ask
      let scheme'bindings = map (second (closeOver . apply subst)) type'bindings
          env' = apply subst $ t'env `Map.union` Map.fromList scheme'bindings
      return env'


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
    Let name value expr -> do
      ex'val <- expand'expr value
      ex'expr <- expand'expr expr
      return $ Let name ex'val ex'expr
    Fix expr -> do
      ex'expr <- expand'expr expr
      return $ Fix ex'expr
    Ann type' expr -> do
      norm'type <- E.evaluate type'
      ex'expr <- expand'expr expr
      return $ Ann norm'type ex'expr
    Intro name exprs ->
      return $ Intro name exprs -- NOTE: I can safely do that - this is generated, no type annotations can get there
    Elim cons'decls val destrs ->
      return $ Elim cons'decls val destrs -- NOTE: I can safely do that - this is generated, no type annotations can get there


-- | NOTE: this can stay like this for now
infer'many :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type])
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
      infer'groups :: [SCC (String, Expression)] -> Analyze ([(String, Type)], [Constraint Type])
      infer'groups [] = return ([], [])
      infer'groups ((AcyclicSCC bind) : sccs) = do
        (t'binds, constrs) <- infer'group [bind]
        (k'env, t'env, ali'env) <- ask
        (t'binds', constrs') <- merge'into't'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
        return (t'binds ++ t'binds', constrs ++ constrs')

      infer'groups ((CyclicSCC bindings) : sccs) = do
        (t'binds, constrs) <- infer'group bindings
        (k'env, t'env, ali'ev) <- ask
        (t'binds', constrs') <- merge'into't'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
        return (t'binds ++ t'binds', constrs ++ constrs')


-- | NOTE: this can stay like this for now
infer'group :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type])
infer'group bindings = do
  let names = map fst bindings
      gener name = do ForAll [] <$> (TyVar <$> fresh)
  fresh'vars <- mapM gener names
  merge'into't'env (zip names fresh'vars) $ infer'many' bindings


-- | NOTE: this can stay like this for now
infer'many' :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type])
infer'many' [] = do
  return ([], [])
infer'many' ((name, expr) : exprs) = do
  (type', constraints) <- infer expr

  orig'type <- lookup't'env name
  (types, constrs') <- infer'many' exprs
  return ((name, type') : types, (orig'type, type') : constraints ++ constrs')


-- runInfer :: AnalyzeEnv -> Analyze (Type, [Constraint Type]) -> Either Error (Type, [Constraint  Type])
-- runInfer env m = runExcept $ evalStateT (runReaderT m env) init'infer


-- run'infer'many :: AnalyzeEnv -> Analyze ([(String, Type)], [Constraint  Type]) -> Either Error ([(String, Type)], [Constraint  Type])
-- run'infer'many env m = runExcept $ evalStateT (runReaderT m env) init'infer
