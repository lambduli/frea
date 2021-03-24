module Compiler.TypeChecker.TypeOf where


import qualified Data.Map.Strict as Map

import Compiler.Syntax.Declaration
import Compiler.Syntax
  ( Bind(..)
  , Declaration(..), ConstrDecl(..)
  , Expression(..)
  , Lit(..)
  , MatchGroup(..), Match(..)
  , Pattern(..)
  , Sig(..)
  , Type(..), Scheme(..))
import Compiler.TypeChecker.Utils
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Inference


infer'env :: [Declaration] -> TypeEnv -> Either TypeError TypeEnv
infer'env binds t'env
  = case sequence eiths of
      Left t'err -> Left t'err
      Right pairs -> Right $ Env $ Map.fromList pairs

    where
      infer'pair :: (TypeEnv, [Either TypeError (String, Scheme)]) -> Declaration -> (TypeEnv, [Either TypeError (String, Scheme)])
      infer'pair (t'env, eiths) (Binding name exp) = case infer'expression t'env exp of
        Left t'err -> (t'env, Left t'err : eiths)
        Right scheme ->
          let Env env'map = t'env
          in (Env $ Map.insert name scheme env'map, Right (name, scheme) : eiths)
      infer'pair acc DataDecl{} = acc

      eiths :: [Either TypeError (String, Scheme)]
      (t'env', eiths) = foldl infer'pair (t'env, []) binds



-- tohle pouziju potom na inferenci listu deklaraci funkci
inferTop :: TypeEnv -> [(String, Expression)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case infer'expression env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs



-- infer'expression :: TypeEnv -> Expression -> Either TypeError Scheme
-- infer'expression env = runInfer . infer env

-- Solve for the toplevel type of an expression in a given environment
infer'expression :: TypeEnv -> Expression -> Either TypeError Scheme
infer'expression env expr = case runInfer env (infer expr) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty




typeof :: Expression -> Either TypeError Scheme
typeof = infer'expression empty'env
