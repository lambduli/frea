module Interpreter.Value where

import Data.List

import Compiler.Syntax.Expression (Expression)
import Compiler.Syntax.Literal


newtype Env = Env [(String, Closed)]

type Closed = (Expression, Env)


data Value
  = Op String
  | Lit Lit
  | Lam String Expression Env
  | Tuple [Value]
  | List [Value]


instance Show Value where
  show (Op name) = name
  show (Lit lit) = show lit
  show (Lam par body env) = "<lambda>"
  show (Tuple values) = "(" ++ intercalate ", " (map show values) ++ ")"
  show (List values) = "[" ++ intercalate ", " (map show values) ++ "]"
