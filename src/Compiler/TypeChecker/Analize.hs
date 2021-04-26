module Compiler.TypeChecker.Analize where


import Compiler.TypeChecker.AnalizeEnv
import Compiler.TypeChecker.AnalizeState


-- Inference monad
type Analize a
  = ReaderT
      AnalizeEnv        -- | Typing environment
      (StateT           -- | Inference state
        AnalizeState
        (Except         -- | Inference errors
          Error))
      a                 -- | Result
