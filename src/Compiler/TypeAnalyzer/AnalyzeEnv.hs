module Compiler.TypeAnalyzer.AnalyzeEnv where


import qualified Data.Map.Strict as Map

import Compiler.Syntax.Type
import Compiler.Syntax.Kind

import Compiler.TypeAnalyzer.Types


data AnalyzeEnv = AEnv { kind'env :: KindEnv, type'env :: TypeEnv, ali'env :: AliEnv }


type KindEnv = Map.Map String Kind


type TypeEnv = Map.Map String Scheme


type AliEnv = Map.Map String Type


empty'an'env :: AnalyzeEnv
empty'an'env = AEnv empty'k'env empty't'env empty'ali'env


-- not really empty
empty't'env :: TypeEnv
empty't'env = Map.fromList
  [ ("#fst",    ForAll ["a", "b"] ((TyTuple [TyVar (TVar "a" Star), TyVar (TVar "b" Star)]) `type'fn` (TyVar (TVar "a" Star))))
  , ("#snd",    ForAll ["a", "b"] ((TyTuple [TyVar (TVar "a" Star), TyVar (TVar "b" Star)]) `type'fn` (TyVar (TVar "b" Star))))
  , ("#=",      ForAll ["a"]      (TyTuple [TyVar (TVar "a" Star), TyVar (TVar "a" Star)] `type'fn` t'Bool))
  , ("#<",      ForAll ["a"]      (TyTuple [TyVar (TVar "a" Star), TyVar (TVar "a" Star)] `type'fn` t'Bool))
  , ("#>",      ForAll ["a"]      (TyTuple [TyVar (TVar "a" Star), TyVar (TVar "a" Star)] `type'fn` t'Bool))
  , ("#+",      ForAll []         (TyTuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#+.",     ForAll []         (TyTuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#*",      ForAll []         (TyTuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#*.",     ForAll []         (TyTuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#-",      ForAll []         (TyTuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#-.",     ForAll []         (TyTuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#div",    ForAll []         (TyTuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#/",      ForAll []         (TyTuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#show",   ForAll ["a"]      (TyVar (TVar "a" Star) `type'fn` (TyApp (TyCon (TCon "List" (KArr Star Star))) t'Char))) -- wiring the List type into the compiler
  , ("#debug",  ForAll ["a"]      (TyVar (TVar "a" Star) `type'fn` TyVar (TVar "a" Star)))
  ]


empty'k'env :: KindEnv
empty'k'env = Map.fromList
  [ ("Bool"   , Star) 
  , ("Int"    , Star) 
  , ("Double" , Star) 
  , ("Char"   , Star) 
  , ("Unit"   , Star)
  , ("(->)"   , Star `KArr` (Star `KArr` Star)) ]


empty'ali'env :: AliEnv
empty'ali'env = Map.empty
