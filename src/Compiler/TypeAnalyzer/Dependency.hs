module Compiler.TypeAnalyzer.Dependency where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Map.Strict ((!), (!?))

import Control.Monad.Except

import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.Syntax.Expression

import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Analyze


index'bindings :: [(String, a)] -> Map.Map String Int -- [((String, Expression), Int)]
index'bindings = enumerate'bindings 0
  where
    enumerate'bindings :: Int -> [(String, a)] -> Map.Map String Int
    enumerate'bindings _ [] = Map.empty
    enumerate'bindings n ((name, expr) : bs) = Map.insert name n $ enumerate'bindings (n + 1) bs


-- | Function declaration dependency analysis
build'graph :: [(String, Expression)] -> Map.Map String Int -> [((String, Expression), Int, [Int])]
build'graph bindings indexer = graph
  where
    get'deps :: Expression -> Set.Set Int
    get'deps expr =
      case expr of
        Var name ->
          maybe Set.empty Set.singleton (indexer !? name)

        Op _ -> Set.empty

        Lit _ -> Set.empty

        Lam par body ->
          case indexer !? par of
            Nothing -> get'deps body
            Just ix -> Set.delete ix $ get'deps body

        App left right ->
          get'deps left `Set.union` get'deps right

        Tuple exprs ->
          foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty exprs

        If b'expr then'expr else'expr ->
          let b'deps = get'deps b'expr
              t'deps = get'deps then'expr
              e'deps = get'deps else'expr
          in  b'deps `Set.union` t'deps `Set.union` e'deps

        Let bind'pairs body'expr ->
          let vs'deps = foldl (\ acc expr -> acc `Set.union` get'deps expr) Set.empty (map snd bind'pairs)
              b'deps  = get'deps body'expr
          in  vs'deps `Set.union` b'deps

        -- this should always yield an empty Set, but just to be sure
        Intro _ exprs -> foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty exprs

        Elim _ expr exprs ->
          foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty (expr : exprs)

        Ann _ expr ->
          get'deps expr


    dependencies = map (get'deps . snd) bindings

    graph = zipWith (\ (name, expr) deps -> ((name, expr), indexer ! name, Set.toList deps)) bindings dependencies


-- | Type synonym dependency analysis
check'for'synonym'cycles :: [Declaration] -> Analyze ()
check'for'synonym'cycles decls = do
  let ds = map to'pair decls
      indexed = index'bindings ds
      graph = build'ali'graph ds indexed
      solved = stronglyConnComp graph
  all'acyclic solved

    where
      -- DUPLICATION
      to'pair :: Declaration -> (String, Type)
      to'pair (TypeAlias name type') = (name, type')

      all'acyclic :: [SCC (String, Type)] -> Analyze ()
      all'acyclic [] = return ()
      all'acyclic ((AcyclicSCC bind) : sccs) =
        all'acyclic sccs
      all'acyclic ((CyclicSCC aliases) : sccs) =
        throwError $ SynonymCycle aliases

      build'ali'graph :: [(String, Type)] -> Map.Map String Int -> [((String, Type), Int, [Int])]
      build'ali'graph bindings indexer = graph
        where
          get'deps :: Type -> Set.Set Int
          get'deps expr =
            case expr of
              TyVar name kind' -> Set.empty
              TyCon name kind' -> maybe Set.empty Set.singleton (indexer !? name)
              TyTuple types -> foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty types
              TyArr t'from t'to -> get'deps t'from `Set.union` get'deps t'to
              TyApp t'left t'right -> get'deps t'left `Set.union` get'deps t'right
              TyOp par t' -> get'deps t'

          dependencies = map (get'deps . snd) bindings

          graph = zipWith (\ (name, expr) deps -> ((name, expr), indexer ! name, Set.toList deps)) bindings dependencies
