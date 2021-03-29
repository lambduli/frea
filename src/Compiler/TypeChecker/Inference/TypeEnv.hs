module Compiler.TypeChecker.Inference.TypeEnv where


import qualified Data.Map.Strict as Map
import Compiler.Syntax.Type


type TypeEnv = Map.Map String Scheme
