module Compiler.TypeAnalyzer.Type.Evaluate where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.State


import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.AnalyzeState
import Compiler.TypeAnalyzer.AnalyzeUtils


class Normalizing a where
  evaluate :: a -> Analyze a


instance Normalizing Type where
  evaluate (TyVar (TVar name k')) = do
    ali'env <- asks ali'env
    case ali'env Map.!? name of
      Nothing -> return $ TyVar (TVar name k')
      Just t -> evaluate t

  evaluate (TyCon (TCon name k')) = do
    ali'env <- asks ali'env
    case ali'env Map.!? name of
      Nothing -> return $ TyCon (TCon name k')
      Just t -> evaluate t

  evaluate (TyTuple types) = do
    n't <- mapM evaluate types
    return $ TyTuple n't

  -- evaluate (TyArr t'from t'to) = do
  --   nt'from <- evaluate t'from
  --   nt'to <- evaluate t'to
  --   return $ nt'from `TyArr` nt'to

  evaluate (TyOp par t') = do
    nt' <- evaluate t'
    return $ TyOp par nt'
  
  evaluate (TyApp t'l t'r) = do
    nt'l <- evaluate t'l
    nt'r <- evaluate t'r
    ty'app nt'l nt'r
  

ty'app :: Type -> Type -> Analyze Type
ty'app op@(TyOp par t'body) t'arg = do
  let free't'vars = Set.map (\ (TVar name _) -> name) $ free'vars t'arg
      -- TODO: NOTE: this is just a quick fix, maybe it would be better to change collect'bound'vars to produce Set TVar
      -- and rename to accept Set TVar instead
      -- I will need to think about it.
      --
      bound'vars = collect'bound'vars op
      reserved'vars = Set.union free't'vars bound'vars
  renamed <- rename reserved'vars op
  ty'app' renamed t'arg
    where
      ty'app' (TyOp par t'body) arg = do
        put'in'ali'env (par, t'arg) (evaluate t'body)

ty'app t'left t'right = return $ TyApp t'left t'right


collect'bound'vars :: Type -> Set.Set String
collect'bound'vars (TyVar (TVar name k')) = Set.singleton name
collect'bound'vars (TyCon (TCon name k')) = Set.empty
collect'bound'vars (TyTuple types) = foldl (\ acc t' -> Set.union acc $ collect'bound'vars t') Set.empty types
-- collect'bound'vars (TyArr t'from t'to) = Set.union (collect'bound'vars t'from) (collect'bound'vars t'to)
collect'bound'vars (TyApp t'left t'right) = Set.union (collect'bound'vars t'left) (collect'bound'vars t'right)
collect'bound'vars (TyOp par type') = Set.insert par $ collect'bound'vars type'


rename :: Set.Set String -> Type -> Analyze Type
rename ftvs (TyOp par body) = do
  new <- real'fresh (Set.toList ftvs) () -- a "guaranteed to be fresh" var name
  k' <- KVar <$> fresh -- one fresh kind variable
  -- TODO: NOTE!
  -- I just figured it should be OK to just create a new kind variable
  -- at this moment, I don't think the correct kind is known
  -- but I might be wrong
  renamed'body <- rename ftvs body -- recursion on the body with the same set of free variables
  return $ TyOp new $ apply (Sub $ Map.singleton (TVar par Star) (TyVar (TVar new k'))) renamed'body -- 
  -- TODO: FIX! The Star and this whole part     ^^^^^^^^^^^^^^^
  -- is just WRONG, I've put it here just so it compiles, but I need to figure out the actual kind of the type variable/parameter
  -- 

rename _ t' = return t'
{- TODO: QUESTION:  Why do I only care about the TyOp when it's top level?
                    What if it's somewhere deeper? Like (TyApp (TyOp _ _) _)?
                    I should definitely investigate later.
-}


remove :: AliEnv -> String -> AliEnv
remove env var = Map.delete var env


extend :: AliEnv -> (String, Type) -> AliEnv
extend env (ty'var, type') = Map.insert ty'var type' env
