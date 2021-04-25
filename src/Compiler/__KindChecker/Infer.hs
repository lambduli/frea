module Compiler.KindChecker.Infer where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Compiler.KindChecker.KindEnv
import Compiler.KindChecker.InferState
import Compiler.KindChecker.KindError




-- Inference monad
type Infer a
  = ReaderT
      KindEnv           -- Kind environment
      (StateT           -- Inference state
        InferState
        (Except         -- Inference errors
          KindError))
      a                 -- Result
