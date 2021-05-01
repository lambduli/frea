module Compiler.TypeChecker.Kind.Infer where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Bifunctor (second)
import Data.Functor.Identity

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.Syntax.Kind

import Compiler.TypeChecker.Error
import Compiler.TypeChecker.Analize
import Compiler.TypeChecker.Solver
import Compiler.TypeChecker.Substituable
import Compiler.TypeChecker.AnalyzeEnv
import Compiler.TypeChecker.Constraint
import Compiler.TypeChecker.AnalyzeState
import Compiler.TypeChecker.AnalyzeUtils



analyze :: Declaration -> Analize (Kind, [Constraint Kind])
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
      infer'constrs :: [ConstrDecl] -> Analize [Constraint Kind]
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


combine'inference :: ([Kind], [Constraint Kind]) -> Type -> Analize ([Kind], [Constraint Kind])
combine'inference (kinds, constrs) t = do
  (k, cs) <- infer'kind t
  return (k : kinds, constrs ++ cs)


kind'of :: AnalizeEnv -> Type -> Either Error Kind
kind'of (k'env, t'env, ali'env) t = do
  case run'infer' (k'env, t'env, ali'env) (infer'kind t) of
    Left err -> Left err
    Right (k, constraints) ->  
      case runSolve constraints of
        Left err -> Left err
        Right subst -> do
          return $ apply subst k
          -- let env' = apply subst $ environment `Map.union` Map.fromList kind'bindings
          -- let env'' = specify'k'vars env'
          -- return env''

infer'kind :: Type -> Analize (Kind, [Constraint Kind])
infer'kind type' =
  case type' of
    TyVar name -> do
      k' <- lookup'k'env name
      return (k', [])
    TyCon name -> do
      k' <- lookup'k'env name
      return (k', [])
    TyTuple types -> do
      -- nez ale reknu ze ten tuple je v poradku
      -- musim projit vsechny types a zkontrolovat, ze jsou kindu *
      -- to se udela tak, ze projdu vsechny types, infernu jim kindy
      -- a pak je zase zipnu se Starem
      (kinds, constrs) <- foldM combine'inference ([], []) types
      let len = length types
      let constraints = zip (replicate len Star) kinds

      return (Star, constraints)
    TyArr left right -> do
      -- tady prijde na radu rekurze
      -- tohle by zrovna melo bejt jednoduchy
      -- left i right musi bejt *
      -- takze je infernu a pak jim priradim v constraintu *
      (k'l, cs'l) <- infer'kind left
      (k'r, cs'r) <- infer'kind right

      return (Star, [(Star, k'l), (Star, k'r)] ++ cs'l ++ cs'r)
    TyApp left right -> do
      -- tohle bude malinko komplikovanejsi
      -- infernu left a infernu right
      -- vytvorim constraint, ze to nalevo musi bejt KArr
      -- ktera bere to napravo a vraci cokoliv - fresh
      (k'l, cs'l) <- infer'kind left
      (k'r, cs'r) <- infer'kind right
      fresh'name <- fresh
      let var = KVar fresh'name
      let constraint = (k'l, k'r `KArr` var)

      return (var, constraint : cs'l ++ cs'r)

    TyOp par t@(TyOp par' type') -> do
      -- tady udelam jenom to, ze reknu, TOHLE je fresh -> to co vypadne z t
      fresh'name <- fresh
      let var = KVar fresh'name
      (k', cs) <- put'in'k'env (par, var) (infer'kind t)
      
      return (var `KArr` k', cs)

    TyOp par t -> do
      -- t must be :: *
      fresh'name <- fresh
      let var = KVar fresh'name
      (k', cs) <- put'in'k'env (par, var) (infer'kind t)

      return (var `KArr` k', (k', Star) : cs)


analyze'types :: AnalizeEnv -> [(String, Declaration )] -> Either Error KindEnv
analyze'types (k'env, t'env, ali'env) bindings =
  case run'infer (k'env, t'env, ali'env) (infer'datas bindings) of
    Left err -> Left err
    Right (kind'bindings, constraints) ->
      case runSolve constraints of
        Left err -> Left err
        Right subst -> do
          let env' = apply subst $ k'env `Map.union` Map.fromList kind'bindings
          let env'' = specify'k'vars env'
          return env''


specify'k'vars :: KindEnv -> KindEnv
specify'k'vars env = Map.map specify'k'vars' env
  where
    specify'k'vars' :: Kind -> Kind
    specify'k'vars' Star = Star
    specify'k'vars' (KVar _) = Star
    specify'k'vars' (KArr left right) = KArr (specify'k'vars' left) (specify'k'vars' right)


run'infer :: AnalizeEnv -> Analize ([(String, Kind)], [Constraint Kind]) -> Either Error ([(String, Kind)], [Constraint Kind])
run'infer env m = runExcept $ evalStateT (runReaderT m env) init'analize


run'infer' :: AnalizeEnv -> Analize (Kind, [Constraint Kind]) -> Either Error (Kind, [Constraint Kind])
run'infer' env m = runExcept $ evalStateT (runReaderT m env) init'analize


infer'datas :: [(String, Declaration)] -> Analize ([(String, Kind)], [Constraint Kind])
infer'datas bindings = do
  -- do env musim pridat pro kazdy jmeno typoveho constructoru
  -- novou neznamou kind var _?
  -- pak muzu ten list bindingu nechat vesele infernout list constraintu
  let names = map fst bindings
      gener name = fresh
  fresh'names <- mapM gener names
  let fresh'vars = map KVar fresh'names
  merge'into'k'env (zip names fresh'vars) $ infer'datas' bindings


infer'datas' :: [(String, Declaration)] -> Analize ([(String, Kind)], [Constraint Kind])
infer'datas' [] = do
  return ([], [])

infer'datas' ((name, d@(DataDecl _ t'pars _)) : decls) = do
  let gener _ = fresh
  fresh'names <- mapM gener t'pars
  let fresh'vars = map KVar fresh'names
  let pairs = zip t'pars fresh'vars
  (kind', constraints) <- merge'into'k'env pairs $ analyze d

  orig'kind <- lookup'k'env name
  (kinds, constrs') <- infer'datas' decls
  return ((name, kind') : kinds, (orig'kind, kind') : constraints ++ constrs')

infer'datas' ((name, a@(TypeAlias _ type')) : decls) = do
  (k', constrs') <- infer'kind type' -- put'in'env (name, Star) (infer'kind type')
  -- jakej kind to bude se pozna podle poctu lambd v type'
  -- ale ja to tam pridavat vubec nemusim, vim jiste, ze jsou acyklicky
  -- takze samotnej type' nepotrebuje vedet jakej kind on sam je
  (ks', constrs) <- put'in'k'env (name, k') (infer'datas' decls)
  -- a ted uz bych mel vedet jakej kind to je - uvnitr k' by ta informace mela bejt
  -- takze zbytek typu muzu odvodit s touhle vedomosti
  return ((name, k') : ks', constrs' ++ constrs)
  -- name nebude Star celkem urcite (vetsinou)
  -- k' taky ne
  -- aby ale telo te type funkce bylo :: *
  -- o to se postara konkretni pravidlo kind'check resp type'infer tady v tom modulu
  -- to pravidlo muze bejt tak, ze pokud telem TyOp je TyOp tak ok - muze to bejt k -> k
  -- ale pokud to je cokoliv jineho, tak to musi bejt *
