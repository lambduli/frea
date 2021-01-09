module Interpreter.Error where

import Compiler.Syntax (Expression)

data EvaluationError
  = UnboundVar String
  -- | WrongNegation Expression
  -- | ApplicationError Expression Expression -- Function Argument
  | BadOperatorApplication String Expression
  | IndexOutOfBound Int
  | NilHeadException
  | NilTailException
  | DivisionByZero Int

instance Show EvaluationError where
  show (UnboundVar name) = "Unknown variable " ++ name
  -- show (WrongNegation exp) = "Incorrect syntactic negation in " ++ show exp
  -- show (ApplicationError left right) = "Bad application of \n    " ++ show left ++ "\n  to \n    " ++ show right
  show (BadOperatorApplication name exp) = "Bad use of the operator " ++ name ++ "\n  in the expression \n    (" ++ name ++ show exp ++ ")"
  show (IndexOutOfBound ind) = "Index out of the bound error. (" ++ show ind ++ ")"
  show NilHeadException = "Native function #head called on an empty list."
  show NilTailException = "Native function #tail called on an empty list."
  show (DivisionByZero left) = "Division by zero error. (" ++ show left ++ " / 0)"
