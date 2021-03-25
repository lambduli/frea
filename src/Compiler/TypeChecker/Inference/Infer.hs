module Compiler.TypeChecker.Inference.Infer where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Compiler.TypeChecker.TypeError
import Compiler.Syntax.Type
import Compiler.TypeChecker.Type


-- Inference monad
type Infer a
  = ReaderT
      TypeEnv           -- Typing environment
      (StateT           -- Inference state
        InferState
        (Except         -- Inference errors
          TypeError))
      a                 -- Result


newtype TypeEnv = Env (Map.Map String Scheme)
  deriving (Show)


type Constraint = (Type, Type)


-- Inference state
newtype InferState
  = InferState { count :: Int }


-- initial inference state
init'infer :: InferState
init'infer = InferState { count = 0 }


-- not really empty
empty'env :: TypeEnv
empty'env = Env $ Map.fromList
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
  ]

runInfer :: TypeEnv -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) init'infer