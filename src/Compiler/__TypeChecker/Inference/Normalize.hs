module Compiler.TypeChecker.Inference.Normalize where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.State


import Compiler.Syntax.Type
import Compiler.TypeChecker.Inference.Norm
import Compiler.TypeChecker.Inference.Substituable

import Debug.Trace


class Normalizing a where
  normalize :: a -> Norm a


instance Normalizing Type where
  normalize (TyVar name) = do
    env <- ask
    case env Map.!? name of
      Nothing -> return $ TyVar name
      Just t -> normalize t
  normalize (TyCon name) = do
    env <- ask
    case env Map.!? name of
      Nothing -> return $ TyCon name
      Just t -> normalize t
  normalize (TyTuple types) = do
    n't <- mapM normalize types
    return $ TyTuple n't
  normalize (TyArr t'from t'to) = do
    nt'from <- normalize t'from
    nt'to <- normalize t'to
    return $ nt'from `TyArr` nt'to
  normalize (TyOp par t') = do
    nt' <- normalize t'
    return $ TyOp par nt'
  
  normalize (TyApp t'l t'r) = do
    nt'l <- normalize t'l
    nt'r <- normalize t'r
    ty'app nt'l nt'r
  

  -- normalize ali'env (TyApp t'l t'r) = ty'app ali'env (normalize ali'env t'l) (normalize ali'env t'r)

ty'app :: Type -> Type -> Norm Type
ty'app op@(TyOp par t'body) t'arg = do
  let free't'vars = ftv t'arg
      bound'vars = collect'bound'vars op
      reserved'vars = Set.union free't'vars bound'vars
  renamed <- rename reserved'vars op
  ty'app' renamed t'arg
    where
      ty'app' (TyOp par t'body) arg = do
        put'in'env (par, t'arg) (normalize t'body)

ty'app t'left t'right = return $ TyApp t'left t'right


collect'bound'vars :: Type -> Set.Set String
collect'bound'vars (TyVar name) = Set.singleton name
collect'bound'vars (TyCon name) = Set.empty
collect'bound'vars (TyTuple types) = foldl (\ acc t' -> Set.union acc $ collect'bound'vars t') Set.empty types
collect'bound'vars (TyArr t'from t'to) = Set.union (collect'bound'vars t'from) (collect'bound'vars t'to)
collect'bound'vars (TyApp t'left t'right) = Set.union (collect'bound'vars t'left) (collect'bound'vars t'right)
collect'bound'vars (TyOp par type') = Set.insert par $ collect'bound'vars type'


rename :: Set.Set String -> Type -> Norm Type
rename ftvs (TyOp par body) = do
  new <- real'fresh ftvs ()
  renamed'body <- rename ftvs body
  return $ TyOp new $ apply (Sub $ Map.singleton par (TyVar new)) renamed'body
rename _ t' = return t'


remove :: AliEnv -> String -> AliEnv
remove env var = Map.delete var env


extend :: AliEnv -> (String, Type) -> AliEnv
extend env (ty'var, type') = Map.insert ty'var type' env


put'in'env :: (String, Type) -> Norm a -> Norm a
put'in'env (var, type') m = do
  let scope env = remove env var `extend` (var, type')
  local scope m


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


real'fresh :: Set.Set String -> a -> Norm String
real'fresh vars var = do
  NormState { count = counter } <- get
  put $ NormState { count = counter + 1 }
  let name = letters !! counter
  if name `Set.member` vars
    then real'fresh vars var
    else return name
