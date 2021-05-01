module Compiler.TypeChecker.AnalyzeUtils where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Kind
import Compiler.Syntax.Type

import Compiler.TypeChecker.Substituable
import Compiler.TypeChecker.AnalyzeEnv
import Compiler.TypeChecker.Analyze
import Compiler.TypeChecker.AnalyzeState
import Compiler.TypeChecker.Error


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
  let scope (k'env, t'env, ali'env) = (k'env, Map.fromList bindings `Map.union` t'env, ali'env)
  local scope m


put'in't'env :: (String, Scheme) -> Analyze a -> Analyze a
put'in't'env (var, scheme) m = do
  -- (k'env, _) <- ask
  let scope (k'env, t'env, ali'env) = (k'env, remove t'env var `extend` (var, scheme), ali'env)
  local scope m


lookup't'env :: String -> Analyze Type
lookup't'env var = do
  (_, env, _) <- ask
  case Map.lookup var env of
    Nothing     ->  throwError $ UnboundVar var
    Just scheme ->  instantiate scheme


merge'into'k'env :: [(String, Kind)] -> Analyze a -> Analyze a
merge'into'k'env bindings m = do
  let scope (k'env, t'env, ali'env) = (Map.fromList bindings `Map.union` k'env, t'env, ali'env)
  local scope m


put'in'k'env :: (String, Kind) -> Analyze a -> Analyze a
put'in'k'env (var, kind') m = do
  let scope (k'env, t'env, ali'env) = (remove k'env var `extend` (var, kind'), t'env, ali'env)
  local scope m


lookup'k'env :: String -> Analyze Kind
lookup'k'env var = do
  (k'env, _, _) <- ask
  case Map.lookup var k'env of
    Nothing     -> throwError $ UnboundVar var
    Just kind'  -> return kind' -- we don't instantiate kinds
    -- TODO: do some normalization or something
    -- change all the free variables in the kind into *


put'in'ali'env :: (String, Type) -> Analyze a -> Analyze a
put'in'ali'env = undefined


instantiate :: Scheme -> Analyze Type
instantiate (ForAll args type') = do
  fresh'strs <- mapM (real'fresh args) args
  let ty'vars = map TyVar fresh'strs
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
    normtype (TyCon a) = TyCon a
    normtype (TyTuple ts) = TyTuple $ map normtype ts
    normtype (TyVar a) =
      case lookup a ord of
        Just x -> TyVar x
        Nothing -> error $ "Type variable " ++ show a ++ " not in the signature."


generalize :: TypeEnv -> Type -> Scheme
generalize env type'
  = ForAll type'args type'
    where
      fvt = free'vars type'
      fve = free'vars env
      type'args = Set.toList $ fvt `Set.difference` fve

