module Interpreter.Command where

import Compiler.Syntax.Expression


newtype Command = Assume [(String, Expression)]
  deriving (Show)
