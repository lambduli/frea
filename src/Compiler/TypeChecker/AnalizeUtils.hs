module Compiler.TypeChecker.AnalizeUtils where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Kind
import Compiler.Syntax.Type

import Compiler.TypeChecker.Substituable
import Compiler.TypeChecker.AnalizeEnv
import Compiler.TypeChecker.Analize
import Compiler.TypeChecker.AnalizeState
import Compiler.TypeChecker.Error


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Analize String
fresh = do
  AnalizeState { count = counter } <- get
  put $ AnalizeState { count = counter + 1 }
  return (letters !! counter)


real'fresh :: [String] -> a -> Analize String
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


merge'into't'env :: [(String, Scheme)] -> Analize a -> Analize a
merge'into't'env bindings m = do
  let scope (k'env, t'env) = (k'env, Map.fromList bindings `Map.union` t'env)
  local scope m


-- | OK?
put'in't'env :: (String, Scheme) -> Analize a -> Analize a
put'in't'env (var, scheme) m = do
  -- (k'env, _) <- ask
  let scope (k'env, t'env) = (k'env, remove t'env var `extend` (var, scheme))
  local scope m


-- | OK?
lookup't'env :: String -> Analize Type
lookup't'env var = do
  (_, env) <- ask
  case Map.lookup var env of
    Nothing     ->  throwError $ UnboundVar var
    Just scheme ->  instantiate scheme


merge'into'k'env :: [(String, Kind)] -> Analize a -> Analize a
merge'into'k'env bindings m = do
  let scope (k'env, t'env) = (Map.fromList bindings `Map.union` k'env, t'env)
  local scope m


put'in'k'env :: (String, Kind) -> Analize a -> Analize a
put'in'k'env (var, kind') m = do
  let scope (k'env, t'env) = (remove k'env var `extend` (var, kind'), t'env)
  local scope m


lookup'k'env :: String -> Analize Kind
lookup'k'env var = do
  (k'env, _) <- ask
  case Map.lookup var k'env of
    Nothing     -> throwError $ UnboundVar var
    Just kind'  -> return kind' -- we don't instantiate kinds
    -- TODO: do some normalization or something
    -- change all the free variables in the kind into *








instantiate :: Scheme -> Analize Type
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

