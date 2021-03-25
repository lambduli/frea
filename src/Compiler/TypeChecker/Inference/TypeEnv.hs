module Compiler.TypeChecker.Inference.TypeEnv where


import qualified Data.Map.Strict as Map
import Compiler.Syntax.Type


newtype TypeEnv = Env (Map.Map String Scheme)
  deriving (Show)
