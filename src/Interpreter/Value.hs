module Interpreter.Value where

import Data.List
import qualified Data.Map.Strict as Map

import Compiler.Syntax.Expression (Expression)
import Compiler.Syntax.Literal

import Interpreter.Address


newtype Env = Env (Map.Map String Address)
  deriving (Show)


type Memory = Map.Map Address Value


data Value
  = Op String
  | Lit Lit
  | Lam String Expression Env
  | Tuple [Value]
  | List [Value]
  | Thunk (Env -> Memory -> Either EvaluationError Value) Env
  | Data String [Value] -- Name of the Constr and list of arguments


class Present a where
  present :: Memory -> a -> String


instance Present Value where
  present _ (Op name) = name
  present _ (Lit lit) = show lit
  present mem (Lam par body env) = "< \\ " ++ par ++ " -> " ++ show body ++ " >"
  present mem (Tuple values) = "(" ++ intercalate ", " (map (present mem) values) ++ ")"
  present mem (List values) = "[" ++ intercalate ", " (map (present mem) values) ++ "]"
  present mem (Thunk force'f env)
    = case force'f env mem of
        Left err -> show err
        Right val -> present mem val
  present mem (Data name [])
    = "Data: " ++ name
  present mem (Data name exprs)
    = "Data: (" ++ name ++ " " ++ unwords (map (present mem) exprs) ++ ")"


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
  show (UnboundVar name) =
    "Unknown variable " ++ name
  show (BadOperatorApplication name exp) =
    "Bad use of the operator " ++ name ++ "\n  in the expression \n    (" ++ name ++ ")" -- show exp ++
  show (IndexOutOfBound ind) =
    "Index out of the bound error. (" ++ show ind ++ ")"
  show NilHeadException =
    "Native function #head called on an empty list."
  show NilTailException =
    "Native function #tail called on an empty list."
  show EmptyStringException =
    "Operation called with an empty string."
  show (DivisionByZero left) =
    "Division by zero error. (" ++ show left ++ " / 0)"
  show (Unexpected message) =
    "Unexpected: " ++ message
