module Compiler.TypeChecker.Inference.InferUtils where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.TypeChecker.Inference.Substituable
import Compiler.TypeChecker.Inference.TypeEnv
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.InferState
import Compiler.TypeChecker.TypeError


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Infer Type
fresh = do
  InferState { count = counter } <- get
  put $ InferState { count = counter + 1 }
  return $ TyVar (letters !! counter)


real'fresh :: [String] -> a -> Infer Type
real'fresh vars var = do
  InferState { count = counter } <- get
  put $ InferState { count = counter + 1 }
  let name = letters !! counter
  if name `elem` vars
    then real'fresh vars var
    else return $ TyVar name


extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend env (ty'var, scheme) = Map.insert ty'var scheme env


remove :: TypeEnv -> String -> TypeEnv
remove env var = Map.delete var env


merge'into'env :: [(String, Scheme)] -> Infer a -> Infer a
merge'into'env bindings m = do
  let scope env = Map.fromList bindings `Map.union` env
  local scope m


put'in'env :: (String, Scheme) -> Infer a -> Infer a
put'in'env (var, scheme) m = do
  let scope env = remove env var `extend` (var, scheme)
  local scope m


lookup'env :: String -> Infer Type
lookup'env var = do
  env <- ask
  case Map.lookup var env of
    Nothing     ->  throwError $ UnboundVariable var
    Just scheme ->  instantiate scheme


instantiate :: Scheme -> Infer Type
instantiate (ForAll args type') = do
  args' <- mapM (real'fresh args) args
  let subst = Sub $ Map.fromList $ zip args args'
  return $ apply subst type'


closeOver :: Type -> Scheme
closeOver = normalize . generalize Map.empty


normalize :: Scheme -> Scheme
normalize (ForAll type'args body) = ForAll (fmap snd ord) (normtype body)
  where
    ord = zip (Set.toList . ftv $ body) letters

    normtype (TyArr a b) = TyArr (normtype a) (normtype b)
    normtype (TyCon a) = TyCon a
    normtype (TyTuple ts) = TyTuple $ map normtype ts
    normtype (TyList t) = TyList $ normtype t
    normtype (TyVar a) =
      case lookup a ord of
        Just x -> TyVar x
        Nothing -> error $ "Type variable " ++ show a ++ " not in the signature."


generalize :: TypeEnv -> Type -> Scheme
generalize env type'
  = ForAll type'args type'
    where
      fvt = ftv type'
      fve = ftv env
      type'args = Set.toList $ fvt `Set.difference` fve
