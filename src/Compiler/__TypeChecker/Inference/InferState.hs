module Compiler.TypeChecker.Inference.InferState where


-- Inference state
newtype InferState
  = InferState { count :: Int }


-- initial inference state
init'infer :: InferState
init'infer = InferState { count = 0 }
