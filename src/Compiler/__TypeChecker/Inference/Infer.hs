module Compiler.TypeChecker.Inference.Infer where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Compiler.TypeChecker.TypeError
import Compiler.Syntax.Type
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


runInfer :: TypeEnv -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) init'infer


run'infer'many :: TypeEnv -> Infer ([(String, Type)], [Constraint]) -> Either TypeError ([(String, Type)], [Constraint])
run'infer'many env m = runExcept $ evalStateT (runReaderT m env) init'infer
