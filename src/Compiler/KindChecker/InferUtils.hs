module Compiler.KindChecker.InferUtils where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Kind

import Compiler.KindChecker.Substituable
import Compiler.KindChecker.KindEnv
import Compiler.KindChecker.Infer
import Compiler.KindChecker.InferState
import Compiler.KindChecker.KindError


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Infer Kind
fresh = do
  InferState { count = counter } <- get
  put $ InferState { count = counter + 1 }
  return $ KVar (letters !! counter)


-- real'fresh :: [String] -> a -> Infer Kind
-- real'fresh vars var = do
--   InferState { count = counter } <- get
--   put $ InferState { count = counter + 1 }
--   let name = letters !! counter
--   if name `elem` vars
--     then real'fresh vars var
--     else return $ TyVar name


extend :: KindEnv -> (String, Kind) -> KindEnv
extend env (ty'var, scheme) = Map.insert ty'var scheme env


remove :: KindEnv -> String -> KindEnv
remove env var = Map.delete var env


merge'into'env :: [(String, Kind)] -> Infer a -> Infer a
merge'into'env bindings m = do
  let scope env = Map.fromList bindings `Map.union` env
  local scope m


put'in'env :: (String, Kind) -> Infer a -> Infer a
put'in'env (var, kind') m = do
  let scope env = remove env var `extend` (var, kind')
  local scope m


lookup'env :: String -> Infer Kind
lookup'env var = do
  env <- ask
  case Map.lookup var env of
    Nothing     -> throwError $ UnboundVariable var
    Just kind'  -> return kind' -- instantiate scheme -- we don't instantiate kinds
    -- TODO: do some normalization or something
    -- change all the free variables in the kind into *

-- instantiate :: Scheme -> Infer Type
-- instantiate (ForAll args type') = do
--   args' <- mapM (real'fresh args) args
--   let subst = Sub $ Map.fromList $ zip args args'
--   return $ apply subst type'


-- closeOver :: Type -> Scheme
-- closeOver = normalize . generalize Map.empty


-- normalize :: Scheme -> Scheme
-- normalize (ForAll type'args body) = ForAll (fmap snd ord) (normtype body)
--   where
--     ord = zip (Set.toList . ftv $ body) letters

--     normtype (TyApp a b) = TyApp (normtype a) (normtype b)
--     normtype (TyArr a b) = TyArr (normtype a) (normtype b)
--     normtype (TyCon a) = TyCon a
--     normtype (TyTuple ts) = TyTuple $ map normtype ts
--     normtype (TyList t) = TyList $ normtype t
--     normtype (TyVar a) =
--       case lookup a ord of
--         Just x -> TyVar x
--         Nothing -> error $ "Type variable " ++ show a ++ " not in the signature."


-- generalize :: TypeEnv -> Type -> Scheme
-- generalize env type'
--   = ForAll type'args type'
--     where
--       fvt = ftv type'
--       fve = ftv env
--       type'args = Set.toList $ fvt `Set.difference` fve
