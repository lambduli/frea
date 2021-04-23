module Compiler.TypeChecker.Inference.Normalize where

import qualified Data.Map.Strict as Map

import Compiler.Syntax.Type

import Debug.Trace


class Normalizing a where
  normalize :: Map.Map String Type -> a -> a


instance Normalizing Type where
  normalize ali'env (TyVar name) =
    case ali'env Map.!? name of
      Nothing -> TyVar name
      Just t -> normalize ali'env t
  normalize ali'env (TyCon name) =
    case ali'env Map.!? name of
      Nothing -> TyCon name
      Just t -> normalize ali'env t
  normalize ali'env (TyTuple types) = TyTuple $ map (normalize ali'env) types
  normalize ali'env (TyArr t'from t'to) = TyArr (normalize ali'env t'from) (normalize ali'env t'to)
  normalize ali'env (TyApp t'l t'r) = ty'app ali'env (normalize ali'env t'l) (normalize ali'env t'r)
  normalize ali'env (TyOp par t') = TyOp par $ normalize ali'env t'

ty'app :: Map.Map String Type -> Type -> Type -> Type
ty'app ali'env (TyOp par t'body) t'arg = normalize (Map.insert par t'arg ali'env) t'body
ty'app _ t'left t'right = TyApp t'left t'right
