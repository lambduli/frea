module Compiler.TypeChecker.Inference.TypeOf where


import Data.Graph (SCC(..), stronglyConnComp)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), (!?))
import Data.Bifunctor
import Control.Monad
import Control.Monad.Reader

import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.Syntax.Expression
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Inference
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.Solver
import Compiler.TypeChecker.Inference.Substituable
import Compiler.TypeChecker.Inference.Constraint
import Compiler.TypeChecker.Inference.TypeEnv
import Compiler.TypeChecker.Inference.InferUtils

import Compiler.KindChecker.KindEnv
import Compiler.KindChecker.Inference hiding (infer)
import Compiler.KindChecker.KindError

import Debug.Trace


infer'env :: [Declaration] -> TypeEnv -> Either TypeError TypeEnv
infer'env binds t'env = do
  let only'funs = filter is'fun binds
      pairs = map to'pair only'funs
  infer'top t'env pairs
    where
      is'fun :: Declaration -> Bool
      is'fun (Binding _ _) = True
      is'fun _ = False

      to'pair :: Declaration -> (String, Expression)
      to'pair (Binding name expr) = (name, expr)


infer'decls :: [Declaration]  -> KindEnv -> Either KindError KindEnv
infer'decls binds k'env = do
  let only'data = filter is'data binds
      data'pairs = map to'pair only'data

  infer'data k'env data'pairs
  
    where
      is'data :: Declaration -> Bool
      is'data (DataDecl _ _ _) = True
      is'data _ = False

      to'pair :: Declaration -> (String, Declaration)
      to'pair d@(DataDecl name _ _) = (name, d)


infer'top :: TypeEnv -> [(String, Expression)] -> Either TypeError TypeEnv
infer'top environment bindings =
  case run'infer'many environment (infer'many bindings) of
    Left err -> Left err
    Right (type'bindings, constraints) -> -- ([(String, Type)], [Constraint])
        case runSolve constraints of
          Left err -> Left err
          Right subst -> do
            let scheme'bindings = map (second (closeOver . apply subst)) type'bindings
                env' = apply subst $ environment `Map.union` Map.fromList scheme'bindings
            return env'


infer'expression :: TypeEnv -> Expression -> Either TypeError Scheme
infer'expression env expr = case runInfer env (infer expr) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty


infer'many :: [(String, Expression)] -> Infer ([(String, Type)], [Constraint])
infer'many bindings = do
  let indexed = index'bindings bindings
  let graph = build'graph bindings indexed
  let solved = stronglyConnComp graph
  -- let ss = trace ("solved  " ++ show solved) solved
  -- ted to mam vyreseny a co musim udelat je
  -- ze projdu celej ten solve list a pro kazdy CyclicSCC [(String, Expression)]
    -- priradim kazdymu jmenu Forall [] <$> fresh
    -- pak vlastne provedu posbirani constraintu
    -- pak je vratim nekam
  -- pro kazdy AcyclicSCC (String, Expression)
    -- tady to Expression nezavisi ani samo na sobe, takze neni potreba to zanaset
    -- jenom to infernu -> posbiram constrainty a type a vratim je nekam vejs
  infer'groups solved
    where
      infer'groups :: [SCC (String, Expression)] -> Infer ([(String, Type)], [Constraint])
      infer'groups [] = return ([], [])
      infer'groups ((AcyclicSCC bind) : sccs) = do
        (t'binds, constrs) <- infer'group [bind]
        t'env <- ask
        (t'binds', constrs') <- merge'into'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
        return (t'binds ++ t'binds', constrs ++ constrs')

      infer'groups ((CyclicSCC bindings) : sccs) = do
        (t'binds, constrs) <- infer'group bindings
        t'env <- ask
        (t'binds', constrs') <- merge'into'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
        return (t'binds ++ t'binds', constrs ++ constrs')


infer'group :: [(String, Expression)] -> Infer ([(String, Type)], [Constraint])
infer'group bindings = do
  let names = map fst bindings
      gener name = do ForAll [] <$> fresh
  fresh'vars <- mapM gener names
  merge'into'env (zip names fresh'vars) $ infer'many' bindings


index'bindings :: [(String, Expression)] -> Map.Map String Int -- [((String, Expression), Int)]
index'bindings = enumerate'bindings 0
  where
    enumerate'bindings :: Int -> [(String, Expression)] -> Map.Map String Int
    enumerate'bindings _ [] = Map.empty
    enumerate'bindings n ((name, expr) : bs) = Map.insert name n $ enumerate'bindings (n + 1) bs


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

        Let name val'expr body'expr ->
          let v'deps = get'deps val'expr
              b'deps = get'deps body'expr
          in  v'deps `Set.union` b'deps

        Fix expr -> get'deps expr

        -- this should always yield an empty Set, but just to be sure
        Intro _ exprs -> foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty exprs

        Elim _ expr exprs ->
          foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty (expr : exprs)


    dependencies = map (get'deps . snd) bindings

    graph = zipWith (\ (name, expr) deps -> ((name, expr), indexer ! name, Set.toList deps)) bindings dependencies


infer'many' :: [(String, Expression)] -> Infer ([(String, Type)], [Constraint])
infer'many' [] = do
  return ([], [])
infer'many' ((name, expr) : exprs) = do
  (type', constraints) <- infer expr

  orig'type <- lookup'env name
  (types, constrs') <- infer'many' exprs
  return ((name, type') : types, (orig'type, type') : constraints ++ constrs')
  
  -- this should actually work
  -- instantiate should do nothing to the fresh type variable because the ForAll
  -- has an empty list of type parameters

typeof :: Expression -> Either TypeError Scheme
typeof = infer'expression empty't'env
