module Compiler.TypeChecker.AnalyzeState where


-- Inference state
newtype AnalyzeState
  = AnalizeState { count :: Int }


-- initial inference state
init'analize :: AnalyzeState
init'analize = AnalizeState { count = 0 }
