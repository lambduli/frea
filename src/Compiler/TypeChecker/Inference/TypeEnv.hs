module Compiler.TypeChecker.Inference.TypeEnv where


import qualified Data.Map.Strict as Map
import Compiler.Syntax.Type
import Compiler.TypeChecker.Type


type TypeEnv = Map.Map String Scheme


-- not really empty
empty't'env :: TypeEnv
empty't'env = Map.fromList
  [ ("#fst",  ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "a")))
  , ("#snd",  ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "b")))

  , ("#=",    ForAll ["a"]      (TyTuple [TyVar "a", TyVar "a"] `TyArr` t'Bool))
  , ("#<",    ForAll ["a"]      (TyTuple [TyVar "a", TyVar "a"] `TyArr` t'Bool))
  , ("#>",    ForAll ["a"]      (TyTuple [TyVar "a", TyVar "a"] `TyArr` t'Bool))

  , ("#+",    ForAll []         (TyTuple [t'Int, t'Int] `TyArr` t'Int))
  , ("#+.",    ForAll []         (TyTuple [t'Double, t'Double] `TyArr` t'Double))

  , ("#*",    ForAll []         (TyTuple [t'Int, t'Int] `TyArr` t'Int))
  , ("#*.",    ForAll []         (TyTuple [t'Double, t'Double] `TyArr` t'Double))

  , ("#-",    ForAll []         (TyTuple [t'Int, t'Int] `TyArr` t'Int))
  , ("#-.",    ForAll []         (TyTuple [t'Double, t'Double] `TyArr` t'Double))

  , ("#div",  ForAll []         (TyTuple [t'Int, t'Int] `TyArr` t'Int))
  , ("#/",    ForAll []         (TyTuple [t'Double, t'Double] `TyArr` t'Double))
  , ("#show", ForAll ["a"]      (TyVar "a" `TyArr` (TyApp (TyCon "List") t'Char))) -- wiring the List type into the compiler
  ]