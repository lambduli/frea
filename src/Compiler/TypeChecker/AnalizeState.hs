module Compiler.TypeChecker.AnalizeState where


-- Inference state
newtype AnalizeState
  = AnalizeState { count :: Int }


-- initial inference state
init'analize :: InferState
init'analize = AnalizeState { count = 0 }
