module Compiler.TypeChecker.Inference.Norm where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Compiler.Syntax.Type
import Compiler.Syntax.Declaration


type AliEnv = Map.Map String Type


-- Inference monad
type Norm a
  = ReaderT
      AliEnv
      (StateT
        NormState
        (Except
          NormError))
      a


data NormError -- don't know what could possibly go wrong


-- Inference state
newtype NormState
  = NormState { count :: Int }


-- initial inference state
init'norm :: NormState
init'norm = NormState { count = 0 }


run'norm :: AliEnv -> Norm a -> Either NormError a
run'norm env m = runExcept $ evalStateT (runReaderT m env) init'norm

