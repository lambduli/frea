module Compiler.TypeChecker.Analyze where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


import Compiler.TypeChecker.AnalyzeEnv
import Compiler.TypeChecker.AnalyzeState
import Compiler.TypeChecker.Error


-- Inference monad
type Analyze a
  = ReaderT
      AnalyzeEnv        -- | Kind Context, Type Context, Type Alias Context
      (StateT           -- | Inference state
        AnalyzeState
        (Except         -- | Inference errors
          Error))
      a                 -- | Result


run'analyze :: AnalyzeEnv -> Analyze a -> Either Error a
run'analyze env m = runExcept $ evalStateT (runReaderT m env) init'analyze
