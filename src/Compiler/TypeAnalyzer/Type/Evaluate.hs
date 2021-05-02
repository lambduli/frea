module Compiler.TypeAnalyzer.Type.Evaluate where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.State


import Compiler.Syntax.Type
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.AnalyzeState
import Compiler.TypeAnalyzer.AnalyzeUtils


class Normalizing a where
  evaluate :: a -> Analyze a


instance Normalizing Type where
  evaluate (TyVar name) = do
    (_, _, ali'env) <- ask
    case ali'env Map.!? name of
      Nothing -> return $ TyVar name
      Just t -> evaluate t

  evaluate (TyCon name) = do
    (_, _, ali'env) <- ask
    case ali'env Map.!? name of
      Nothing -> return $ TyCon name
      Just t -> evaluate t

  evaluate (TyTuple types) = do
    n't <- mapM evaluate types
    return $ TyTuple n't

  evaluate (TyArr t'from t'to) = do
    nt'from <- evaluate t'from
    nt'to <- evaluate t'to
    return $ nt'from `TyArr` nt'to

  evaluate (TyOp par t') = do
    nt' <- evaluate t'
    return $ TyOp par nt'
  
  evaluate (TyApp t'l t'r) = do
    nt'l <- evaluate t'l
    nt'r <- evaluate t'r
    ty'app nt'l nt'r
  

ty'app :: Type -> Type -> Analyze Type
ty'app op@(TyOp par t'body) t'arg = do
  let free't'vars = free'vars t'arg
      bound'vars = collect'bound'vars op
      reserved'vars = Set.union free't'vars bound'vars
  renamed <- rename reserved'vars op
  ty'app' renamed t'arg
    where
      ty'app' (TyOp par t'body) arg = do
        put'in'ali'env (par, t'arg) (evaluate t'body)

ty'app t'left t'right = return $ TyApp t'left t'right


collect'bound'vars :: Type -> Set.Set String
collect'bound'vars (TyVar name) = Set.singleton name
collect'bound'vars (TyCon name) = Set.empty
collect'bound'vars (TyTuple types) = foldl (\ acc t' -> Set.union acc $ collect'bound'vars t') Set.empty types
collect'bound'vars (TyArr t'from t'to) = Set.union (collect'bound'vars t'from) (collect'bound'vars t'to)
collect'bound'vars (TyApp t'left t'right) = Set.union (collect'bound'vars t'left) (collect'bound'vars t'right)
collect'bound'vars (TyOp par type') = Set.insert par $ collect'bound'vars type'


rename :: Set.Set String -> Type -> Analyze Type
rename ftvs (TyOp par body) = do
  new <- real'fresh (Set.toList ftvs) ()
  renamed'body <- rename ftvs body
  return $ TyOp new $ apply (Sub $ Map.singleton par (TyVar new)) renamed'body
rename _ t' = return t'


remove :: AliEnv -> String -> AliEnv
remove env var = Map.delete var env


extend :: AliEnv -> (String, Type) -> AliEnv
extend env (ty'var, type') = Map.insert ty'var type' env
