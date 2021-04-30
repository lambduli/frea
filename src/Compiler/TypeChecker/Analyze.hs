module Compiler.TypeChecker.Analyze where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


import Compiler.TypeChecker.AnalyzeEnv
import Compiler.TypeChecker.AnalyzeState
import Compiler.TypeChecker.Error


-- Inference monad
type Analize a
  = ReaderT
      AnalizeEnv        -- | Typing environment
      (StateT           -- | Inference state
        AnalyzeState
        (Except         -- | Inference errors
          Error))
      a                 -- | Result
