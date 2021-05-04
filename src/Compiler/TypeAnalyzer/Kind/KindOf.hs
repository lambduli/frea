module Compiler.TypeAnalyzer.Kind.KindOf where


import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Declaration
import Compiler.Syntax

import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.Constraint
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.Solver
import Compiler.TypeAnalyzer.AnalyzeState
import Compiler.TypeAnalyzer.AnalyzeUtils

-- import qualified Compiler.TypeAnalyzer.Type.Evaluate as E

import Compiler.TypeAnalyzer.Kind.Infer


-- | NOTE:  I thought I should normalize the type before doing the kind inference.
-- |        But it's probably not needed.
-- kind'of :: AnalyzeEnv -> Type -> Either Error Kind
-- kind'of (k'env, t'env, ali'env) t = do
--   let ex't = run'analyze (k'env, t'env, ali'env) (E.evaluate t)
--   case ex't of
--     Left err -> Left err
--     Right t' ->
--       case run'infer (k'env, t'env, ali'env) (infer t') of
--         Left err -> Left err
--         Right (k, constraints) ->  
--           case runSolve constraints of
--             Left err -> Left err
--             Right subst -> do
--               return $ apply subst k
--               -- let env' = apply subst $ environment `Map.union` Map.fromList kind'bindings
--               -- let env'' = specify'k'vars env'
--               -- return env''


kind'of :: AnalyzeEnv -> Type -> Either Error Kind
kind'of (k'env, t'env, ali'env) t = do
  case run'infer (k'env, t'env, ali'env) (infer t) of
    Left err -> Left err
    Right (k, constraints) ->  
      case runSolve constraints of
        Left err -> Left err
        Right subst -> do
          return $ apply subst k
          -- let env' = apply subst $ environment `Map.union` Map.fromList kind'bindings
          -- let env'' = specify'k'vars env'
          -- return env''


-- TODO: refactor to be Analyze so it won't need to use run'infer
-- analyze'type'decls :: AnalyzeEnv -> [(String, Declaration )] -> Either Error KindEnv
-- analyze'type'decls (k'env, t'env, ali'env) bindings =
analyze'type'decls :: [(String, Declaration)] -> [Constraint Kind] -> Analyze KindEnv
analyze'type'decls bindings k'constrs = do
  (kind'bindings, constraints) <- analyze'types bindings
  case runSolve (constraints ++ k'constrs) of
    Left err -> throwError err
    Right subst -> do
      (k'env, _, _) <- ask
      let k'env' = specify'k'vars $ apply subst $ k'env `Map.union` Map.fromList kind'bindings
      return k'env'


specify'k'vars :: KindEnv -> KindEnv
specify'k'vars = Map.map specify'k'vars'
  where
    specify'k'vars' :: Kind -> Kind
    specify'k'vars' Star = Star
    specify'k'vars' (KVar _) = Star
    specify'k'vars' (KArr left right) = KArr (specify'k'vars' left) (specify'k'vars' right)


-- run'infer :: AnalyzeEnv -> Analyze ([(String, Kind)], [Constraint Kind]) -> Either Error ([(String, Kind)], [Constraint Kind])
-- run'infer env m = runExcept $ evalStateT (runReaderT m env) init'analyze


run'infer :: AnalyzeEnv -> Analyze a -> Either Error a
run'infer env m = runExcept $ evalStateT (runReaderT m env) init'analyze


analyze'types :: [(String, Declaration)] -> Analyze ([(String, Kind)], [Constraint Kind])
analyze'types bindings = do
  -- do env musim pridat pro kazdy jmeno typoveho constructoru
  -- novou neznamou kind var _?
  -- pak muzu ten list bindingu nechat vesele infernout list constraintu
  let names = map fst bindings
      gener name = fresh
  fresh'names <- mapM gener names
  let fresh'vars = map KVar fresh'names
  merge'into'k'env (zip names fresh'vars) $ analyze'types' bindings


analyze'types' :: [(String, Declaration)] -> Analyze ([(String, Kind)], [Constraint Kind])
analyze'types' [] = do
  return ([], [])

analyze'types' ((name, d@(DataDecl _ t'pars _)) : decls) = do
  let gener _ = fresh
  fresh'names <- mapM gener t'pars
  let fresh'vars = map KVar fresh'names
  let pairs = zip t'pars fresh'vars
  (kind', constraints) <- merge'into'k'env pairs $ analyze d

  orig'kind <- lookup'k'env name
  (kinds, constrs') <- analyze'types' decls
  return ((name, kind') : kinds, (orig'kind, kind') : constraints ++ constrs')

analyze'types' ((name, a@(TypeAlias _ type')) : decls) = do
  (k', constrs') <- infer type' -- put'in'env (name, Star) (infer'kind type')
  -- jakej kind to bude se pozna podle poctu lambd v type'
  -- ale ja to tam pridavat vubec nemusim, vim jiste, ze jsou acyklicky
  -- takze samotnej type' nepotrebuje vedet jakej kind on sam je
  (ks', constrs) <- put'in'k'env (name, k') (analyze'types' decls)
  -- a ted uz bych mel vedet jakej kind to je - uvnitr k' by ta informace mela bejt
  -- takze zbytek typu muzu odvodit s touhle vedomosti
  return ((name, k') : ks', constrs' ++ constrs)
  -- name nebude Star celkem urcite (vetsinou)
  -- k' taky ne
  -- aby ale telo te type funkce bylo :: *
  -- o to se postara konkretni pravidlo kind'check resp type'infer tady v tom modulu
  -- to pravidlo muze bejt tak, ze pokud telem TyOp je TyOp tak ok - muze to bejt k -> k
  -- ale pokud to je cokoliv jineho, tak to musi bejt *


analyze :: Declaration -> Analyze (Kind, [Constraint Kind])
analyze (DataDecl t'name t'params constructors) = do
  -- vsechny t'params uz jsou v envu jako fresh vars
  -- t'name uz tam je jako fresh kind var
  -- ale ted uz navic vim, ze t'name -> fresh -> fresh -> ... -> fresh -> *
  -- tomu budu rikat main'constr
  -- 
  -- dal pak projdu vsechny constructory
  -- a diky tomu, ze vim, ze kazdy type vlistu typu pro kazdy constructor
  -- musi byt kind * (protoze jinak nemas jak vyrobit hodnotu neceho, co neni kind *)
  -- vzdycky prohlasim, tenhle type je kind *
  -- a pokud to budou TyApp nebo TyArr tak se budu muset zanorit
  -- 
  -- jeden constraint bude vypadat takhle
  orig't'name <- lookup'k'env t'name
  -- ted musim posbirat vsechny kindy vsech t'params do listsu
  -- pak je vlastne akorat poskladam do KArr
  -- a prohlasim, ze orig't'name musi byt presne tahle KArr
  orig't'params <- mapM lookup'k'env t'params
  let k'of'this = foldr KArr Star orig't'params
      main'constr = (orig't'name, k'of'this)
  constrs <- infer'constrs constructors
  let constraints = main'constr : constrs

  return (orig't'name, constraints)

    where
      infer'constrs :: [ConstrDecl] -> Analyze [Constraint Kind]
      infer'constrs [] = return []
      infer'constrs ((ConDecl con'n types) : cons) = do
        -- jak jsem rekl nahore
        -- kazdy prvek listu types musi mit kind *
        -- to vyuzuju k tomu, ze pro kazdy z nich infernu jejich kind
        -- a vytvorim constraint rikajici, ze ten kind musi byt *
        (kinds, constrs) <- foldM combine'inference ([], []) types
        let len = length types
        let constraints = zip (replicate len Star) kinds
        constrs'rest <- infer'constrs cons
        return $ constraints ++ constrs ++ constrs'rest
