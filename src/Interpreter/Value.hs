module Interpreter.Value where

import Data.List
import qualified Data.Map.Strict as Map

import Compiler.Syntax.Expression (Expression)
import Compiler.Syntax.Literal


newtype Env = Env (Map.Map String Closed) -- [(String, Closed)]
  deriving (Show)

type Closed = (Expression, Env)


data Value
  = Op String
  | Lit Lit
  | Lam String Expression Env
  | Tuple [Value]
  | List [Value]
  | Thunk (() -> Either EvaluationError Value)
  | Data String [Value] -- Name of the Constr and list of arguments


instance Show Value where
  show (Op name) = name
  show (Lit lit) = show lit
  show (Lam par body env) = "<lambda>"
  show (Tuple values) = "(" ++ intercalate ", " (map show values) ++ ")"
  show (List values) = "[" ++ intercalate ", " (map show values) ++ "]"
  show (Thunk force'f)
    = case force'f () of
        Left err -> show err
        Right val -> show val
  show (Data name values)
    = "(" ++ name ++ " " ++ unwords (map show values) ++ ")"


data EvaluationError
  = UnboundVar String
  | BadOperatorApplication String Value
  | IndexOutOfBound Int
  | NilHeadException
  | NilTailException
  | EmptyStringException
  | DivisionByZero Int
  | Unexpected String

instance Show EvaluationError where
  show (UnboundVar name) = "Unknown variable " ++ name
  show (BadOperatorApplication name exp) = "Bad use of the operator " ++ name ++ "\n  in the expression \n    (" ++ name ++ show exp ++ ")"
  show (IndexOutOfBound ind) = "Index out of the bound error. (" ++ show ind ++ ")"
  show NilHeadException = "Native function #head called on an empty list."
  show NilTailException = "Native function #tail called on an empty list."
  show EmptyStringException = "Operation called with an empty string."
  show (DivisionByZero left) = "Division by zero error. (" ++ show left ++ " / 0)"
  show (Unexpected message) = "Unexpected: " ++ message
