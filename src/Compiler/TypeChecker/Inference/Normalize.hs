module Compiler.TypeChecker.Inference.Normalize where

import qualified Data.Map.Strict as Map

import Compiler.Syntax.Type


class Normalizing a where
  normalize :: Map.Map String Type -> a -> a


instance Normalizing Type where
  normalize ali'env (TyVar name) = TyVar name
  normalize ali'env (TyCon name) =
    case ali'env Map.!? name of
      Nothing -> TyCon name
      Just t -> normalize ali'env t
  normalize ali'env (TyTuple types) = TyTuple $ map (normalize ali'env) types
  normalize ali'env (TyArr t'from t'to) = TyArr (normalize ali'env t'from) (normalize ali'env t'to)
  normalize ali'env (TyApp t'l t'r) = TyApp (normalize ali'env t'l) (normalize ali'env t'r)