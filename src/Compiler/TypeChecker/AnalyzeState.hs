module Compiler.TypeChecker.AnalyzeState where


-- Inference state
newtype AnalyzeState
  = AnalizeState { count :: Int }


-- initial inference state
init'analyze :: AnalyzeState
init'analyze = AnalizeState { count = 0 }
