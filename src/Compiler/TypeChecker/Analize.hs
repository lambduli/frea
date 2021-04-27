module Compiler.TypeChecker.Analize where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


import Compiler.TypeChecker.AnalizeEnv
import Compiler.TypeChecker.AnalizeState
import Compiler.TypeChecker.Error


-- Inference monad
type Analize a
  = ReaderT
      AnalizeEnv        -- | Typing environment
      (StateT           -- | Inference state
        AnalizeState
        (Except         -- | Inference errors
          Error))
      a                 -- | Result
