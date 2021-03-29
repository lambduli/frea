module Compiler.TypeChecker.Inference.Infer where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Compiler.TypeChecker.TypeError
import Compiler.Syntax.Type
import Compiler.TypeChecker.Type
import Compiler.TypeChecker.Inference.Constraint
import Compiler.TypeChecker.Inference.TypeEnv
import Compiler.TypeChecker.Inference.InferState


-- Inference monad
type Infer a
  = ReaderT
      TypeEnv           -- Typing environment
      (StateT           -- Inference state
        InferState
        (Except         -- Inference errors
          TypeError))
      a                 -- Result


-- not really empty
empty't'env :: TypeEnv
empty't'env = Map.fromList
  [ ("#fst",  ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "a")))
  , ("#snd",  ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "b")))

  , ("#&&",   ForAll ["a"]      (TyTuple [t'Bool, t'Bool] `TyArr` t'Bool))
  , ("#||",   ForAll ["a"]      (TyTuple [t'Bool, t'Bool] `TyArr` t'Bool))
  
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
  , ("#++",   ForAll ["a"]      (TyTuple [TyList (TyVar "a"), TyList (TyVar "a")] `TyArr` TyList (TyVar "a")))
  -- prepend element to a list
  , ("#:",    ForAll ["a"]      (TyTuple [TyVar "a", TyList (TyVar "a")] `TyArr` TyList (TyVar "a")))
  , ("#head", ForAll ["a"]      (TyList (TyVar "a") `TyArr` TyVar "a"))
  , ("#tail", ForAll ["a"]      (TyList (TyVar "a") `TyArr` TyList (TyVar "a")))
  , ("#nil?", ForAll ["a"]      (TyList (TyVar "a") `TyArr` t'Bool))
  , ("#show", ForAll ["a"]      (TyVar "a" `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showint", ForAll []      ((TyCon "Int") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showbool", ForAll []     ((TyCon "Bool") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showdouble", ForAll []   ((TyCon "Double") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showchar", ForAll []     ((TyCon "Char") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showstring", ForAll []   ((TyList $ TyCon "Char") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showunit", ForAll []     ((TyCon "Unit") `TyArr` (TyList (TyCon "Char"))))
  ]


runInfer :: TypeEnv -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) init'infer


run'infer'many :: TypeEnv -> Infer ([(String, Type)], [Constraint]) -> Either TypeError ([(String, Type)], [Constraint])
run'infer'many env m = runExcept $ evalStateT (runReaderT m env) init'infer
