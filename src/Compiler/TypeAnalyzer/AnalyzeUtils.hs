module Compiler.TypeAnalyzer.AnalyzeUtils where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Kind
import Compiler.Syntax.Type

import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeState
import Compiler.TypeAnalyzer.Error


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Analyze String
fresh = do
  AnalizeState { count = counter } <- get
  put $ AnalizeState { count = counter + 1 }
  return (letters !! counter)


real'fresh :: [String] -> a -> Analyze String
real'fresh vars var = do
  AnalizeState { count = counter } <- get
  put $ AnalizeState { count = counter + 1 }
  let name = letters !! counter
  if name `elem` vars
    then real'fresh vars var
    else return name


extend :: (Ord a) => Map.Map a b -> (a, b) -> Map.Map a b
extend env (ty'var, scheme) = Map.insert ty'var scheme env


remove :: (Ord a) => Map.Map a b -> a -> Map.Map a b
remove env var = Map.delete var env


merge'into't'env :: [(String, Scheme)] -> Analyze a -> Analyze a
merge'into't'env bindings m = do
  let scope e@AEnv{ type'env = t'env } = e{ type'env = Map.fromList bindings `Map.union` t'env}
    -- (k'env, Map.fromList bindings `Map.union` t'env, ali'env)
  local scope m


put'in't'env :: (String, Scheme) -> Analyze a -> Analyze a
put'in't'env (var, scheme) m = do
  -- (k'env, _) <- ask
  let scope e@AEnv{ type'env = t'env } = e{ type'env = remove t'env var `extend` (var, scheme) }
    -- (k'env, remove t'env var `extend` (var, scheme), ali'env)
  local scope m


lookup't'env :: String -> Analyze Type
lookup't'env var = do
  env <- asks type'env
  case Map.lookup var env of
    Nothing     ->  throwError $ UnboundVar var
    Just scheme ->  instantiate scheme


merge'into'k'env :: [(String, Kind)] -> Analyze a -> Analyze a
merge'into'k'env bindings m = do
  let scope e@AEnv{ kind'env = k'env } = e{ kind'env = Map.fromList bindings `Map.union` k'env }
    -- (Map.fromList bindings `Map.union` k'env, t'env, ali'env)
  local scope m


put'in'k'env :: (String, Kind) -> Analyze a -> Analyze a
put'in'k'env (var, kind') m = do
  let scope e@AEnv{ kind'env = k'env } = e{ kind'env = remove k'env var `extend` (var, kind') }
    -- (remove k'env var `extend` (var, kind'), t'env, ali'env)
  local scope m


lookup'k'env :: String -> Analyze Kind
lookup'k'env var = do
  k'env <- asks kind'env
  case Map.lookup var k'env of
    Nothing     -> throwError $ UnboundTypeVar var
    Just kind'  -> return kind' -- we don't instantiate kinds
    -- TODO: do some normalization or something
    -- change all the free variables in the kind into *


put'in'ali'env :: (String, Type) -> Analyze a -> Analyze a
put'in'ali'env (name, type') m = do
  let scope e@AEnv{ ali'env = a'env } = e{ ali'env = remove a'env name `extend` (name, type') }
  local scope m


instantiate :: Scheme -> Analyze Type
instantiate (ForAll args type') = do
  fresh'strs <- mapM (real'fresh args) args
  let ty'vars = map (\ name -> TyVar name Star) fresh'strs -- TODO: the Star kind is incorrect
  -- it needs to be fixed promptly
  -- instead -> `args` will (have to) contain information about which parametrized (quantified) variables have which kinds
  --
  let subst = Sub $ Map.fromList $ zip args ty'vars
  return $ apply subst type'


closeOver :: Type -> Scheme
closeOver = normalize . generalize Map.empty


normalize :: Scheme -> Scheme
normalize (ForAll type'args body) = ForAll (fmap snd ord) (normtype body)
  where
    ord = zip (Set.toList . free'vars $ body) letters

    normtype (TyApp a b) = TyApp (normtype a) (normtype b)
    normtype (TyArr a b) = TyArr (normtype a) (normtype b)
    normtype (TyCon a k') = TyCon a k'
    normtype (TyTuple ts) = TyTuple $ map normtype ts
    normtype (TyVar a k') =
      case lookup a ord of
        Just x -> TyVar x k'
        Nothing -> error $ "Type variable " ++ show a ++ " not in the signature."


generalize :: TypeEnv -> Type -> Scheme
generalize env type'
  = ForAll type'args type'
    where
      fvt = free'vars type'
      fve = free'vars env
      type'args = Set.toList $ fvt `Set.difference` fve
