module Compiler.TypeAnalyzer.Kind.Infer where

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

import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.Solver
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Constraint
import Compiler.TypeAnalyzer.AnalyzeState
import Compiler.TypeAnalyzer.AnalyzeUtils


infer :: Type -> Analyze (Kind, [Constraint Kind])
infer type' =
  case type' of
    TyVar name kind' -> do
      k' <- lookup'k'env name
      return (k', [])
      -- TODO: NOTE: should I take the kind from the lookup-ing or from the TyVar value?
    TyCon name kind' -> do
      k' <- lookup'k'env name
      return (k', [])
      -- TODO: NOTE: should I take the kind from the lookup-ing or from the TyCon value?
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
      (k'l, cs'l) <- infer left
      (k'r, cs'r) <- infer right

      return (Star, [(Star, k'l), (Star, k'r)] ++ cs'l ++ cs'r)
    TyApp left right -> do
      -- tohle bude malinko komplikovanejsi
      -- infernu left a infernu right
      -- vytvorim constraint, ze to nalevo musi bejt KArr
      -- ktera bere to napravo a vraci cokoliv - fresh
      (k'l, cs'l) <- infer left
      (k'r, cs'r) <- infer right
      fresh'name <- fresh
      let var = KVar fresh'name
      let constraint = (k'l, k'r `KArr` var)

      return (var, constraint : cs'l ++ cs'r)

    TyOp par t@(TyOp par' type') -> do
      -- tady udelam jenom to, ze reknu, TOHLE je fresh -> to co vypadne z t
      fresh'name <- fresh
      let var = KVar fresh'name
      (k', cs) <- put'in'k'env (par, var) (infer t)
      
      return (var `KArr` k', cs)

    TyOp par t -> do
      -- t must be :: *
      fresh'name <- fresh
      let var = KVar fresh'name
      (k', cs) <- put'in'k'env (par, var) (infer t)

      return (var `KArr` k', (k', Star) : cs)


combine'inference :: ([Kind], [Constraint Kind]) -> Type -> Analyze ([Kind], [Constraint Kind])
combine'inference (kinds, constrs) t = do
  (k, cs) <- infer t
  return (k : kinds, constrs ++ cs)
