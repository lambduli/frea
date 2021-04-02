module Compiler.KindChecker.Inference where

import qualified Data.Map.Strict as Map
import Data.List (elem, foldl)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Bifunctor (second)
import Data.Functor.Identity

import Compiler.Syntax.Kind
import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.KindChecker.KindError
import Compiler.KindChecker.Infer
import Compiler.KindChecker.Solver
import Compiler.KindChecker.Substituable
import Compiler.KindChecker.KindEnv
import Compiler.KindChecker.Constraint
import Compiler.KindChecker.InferState
import Compiler.KindChecker.InferUtils


infer :: Declaration -> Infer (Kind, [Constraint])
infer (DataDecl t'name t'params constructors) = do
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
  orig't'name <- lookup'env t'name
  -- ted musim posbirat vsechny kindy vsech t'params do listsu
  -- pak je vlastne akorat poskladam do KArr
  -- a prohlasim, ze orig't'name musi byt presne tahle KArr
  orig't'params <- mapM (\ par'name -> lookup'env par'name) t'params
  let k'of'this = foldr KArr Star orig't'params
      main'constr = (orig't'name, k'of'this)
  constrs <- infer'constrs constructors
  let constraints = main'constr : constrs

  return (orig't'name, constraints)

    where
      infer'constrs :: [ConstrDecl] -> Infer [Constraint]
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

      combine'inference :: ([Kind], [Constraint]) -> Type -> Infer ([Kind], [Constraint])
      combine'inference (kinds, constrs) t = do
        (k, cs) <- infer'type t
        return (k : kinds, constrs ++ cs)

      infer'type :: Type -> Infer (Kind, [Constraint])
      infer'type type' =
        case type' of
          TyVar name -> do
            k' <- lookup'env name
            return (k', [])
          TyCon name -> do
            k' <- lookup'env name
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
          TyList type' -> do
            -- tady musim to co u tuplu udelat u jedinyho typu
            (k, cs) <- infer'type type'
            return (Star, (Star, k) : cs)
          TyArr left right -> do
            -- tady prijde na radu rekurze
            -- tohle by zrovna melo bejt jednoduchy
            -- left i right musi bejt *
            -- takze je infernu a pak jim priradim v constraintu *
            (k'l, cs'l) <- infer'type left
            (k'r, cs'r) <- infer'type right

            return (Star, [(Star, k'l), (Star, k'r)] ++ cs'l ++ cs'r)
          TyApp left right -> do
            -- tohle bude malinko komplikovanejsi
            -- infernu left a infernu right
            -- vytvorim constraint, ze to nalevo musi bejt KArr
            -- ktera bere to napravo a vraci cokoliv - fresh
            (k'l, cs'l) <- infer'type left
            (k'r, cs'r) <- infer'type right
            var <- fresh
            let constraint = (k'l, k'r `KArr` var)

            return (var, constraint : cs'l ++ cs'r)


infer'data :: KindEnv -> [(String, Declaration )] -> Either KindError KindEnv
infer'data environment bindings =
  case run'infer environment (infer'datas bindings) of
    Left err -> Left err
    Right (kind'bindings, constraints) ->
      case runSolve constraints of
        Left err -> Left err
        Right subst -> do
          let env' = apply subst $ environment `Map.union` Map.fromList kind'bindings
          let env'' = assume'star env'
          return env''


assume'star :: KindEnv -> KindEnv
assume'star env = Map.map assume'star' env
  where
    assume'star' :: Kind -> Kind
    assume'star' Star = Star
    assume'star' (KVar _) = Star
    assume'star' (KArr left right) = KArr (assume'star' left) (assume'star' right)


run'infer :: KindEnv -> Infer ([(String, Kind)], [Constraint]) -> Either KindError ([(String, Kind)], [Constraint])
run'infer env m = runExcept $ evalStateT (runReaderT m env) init'infer


infer'datas :: [(String, Declaration)] -> Infer ([(String, Kind)], [Constraint])
infer'datas bindings = do
  -- do env musim pridat pro kazdy jmeno typoveho constructoru
  -- novou neznamou kind var _?
  -- pak muzu ten list bindingu nechat vesele infernout list constraintu
  let names = map fst bindings
      gener name = fresh
  fresh'vars <- mapM gener names
  merge'into'env (zip names fresh'vars) $ infer'datas' bindings


infer'datas' :: [(String, Declaration)] -> Infer ([(String, Kind)], [Constraint])
infer'datas' [] = do
  return ([], [])
infer'datas' ((name, d@(DataDecl _ t'pars _)) : decls) = do
  let gener _ = fresh
  fresh'vars <- mapM gener t'pars
  let pairs = zip t'pars fresh'vars
  (kind', constraints) <- merge'into'env pairs $ infer d

  orig'kind <- lookup'env name
  (kinds, constrs') <- infer'datas' decls
  return ((name, kind') : kinds, (orig'kind, kind') : constraints ++ constrs')
