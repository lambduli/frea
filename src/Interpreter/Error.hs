module Interpreter.Error where

import Compiler.Syntax (Expression)
import Interpreter.Value

data EvaluationError
  = UnboundVar String
  | BadOperatorApplication String Value
  | IndexOutOfBound Int
  | NilHeadException
  | NilTailException
  | DivisionByZero Int

instance Show EvaluationError where
  show (UnboundVar name) = "Unknown variable " ++ name
  show (BadOperatorApplication name exp) = "Bad use of the operator " ++ name ++ "\n  in the expression \n    (" ++ name ++ show exp ++ ")"
  show (IndexOutOfBound ind) = "Index out of the bound error. (" ++ show ind ++ ")"
  show NilHeadException = "Native function #head called on an empty list."
  show NilTailException = "Native function #tail called on an empty list."
  show (DivisionByZero left) = "Division by zero error. (" ++ show left ++ " / 0)"
