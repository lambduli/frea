module Interpreter.Command where

import Compiler.Syntax.Expression
import Compiler.Syntax (Type(..), Scheme(..))


data Command
  = Define [(String, Expression)]
  | Data String [Constr]
    deriving (Show)


data Constr
  = Con String [Type]
  deriving (Show)
