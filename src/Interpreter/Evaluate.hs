module Interpreter.Evaluate where


import qualified Data.Map as Map

import Compiler.Syntax.Expression
import Compiler.Syntax.Literal


type Env = Map.Map String Expression

data EvaluationError
  = UnboundVar String
  | WrongNegation Expression
  | ApplicationError Expression Expression -- Function Argument
  | BadOperatorApplication String Expression
  deriving (Show)

eval :: Expression -> Either EvaluationError Expression
eval expr = evaluate expr Map.empty

evaluate :: Expression -> Env -> Either EvaluationError Expression
evaluate expr env = case expr of
  Var name -> case Map.lookup name env of
    Nothing -> Left $ UnboundVar name
    Just exp -> Right exp
  Op name -> Right $ Op name
  -- Con name -> Right $ Con name
  Lit lit -> Right $ Lit lit
  Lam arg body -> Right $ Lam arg body
  App (Lam arg body) right -> case evaluate right env of
    Left err -> Left err
    Right val -> evaluate body (Map.insert arg val env)
  App (Op op) right -> apply'operator op right
    -- App (Con arg) right -> 
  NegApp expr -> case evaluate expr env of
    Right (Lit (LitInt i)) -> Right $ Lit (LitInt (-i))
    Right (Lit (LitDouble d)) -> Right $ Lit (LitDouble (-d))
    Left err -> Left err
    _ -> Left $ WrongNegation expr


apply'operator :: String -> Expression -> Either EvaluationError Expression
apply'operator "#=" (Tuple [Lit (LitInt i'l), Lit (LitInt i'r)])
  = Right $ Lit (LitBool (i'l == i'r))
apply'operator "#<" (Tuple [Lit (LitInt i'l), Lit (LitInt i'r)])
  = Right $ Lit (LitBool (i'l < i'r))
apply'operator "#>" (Tuple [Lit (LitInt i'l), Lit (LitInt i'r)])
  = Right $ Lit (LitBool (i'l > i'r))
apply'operator "#+" (Tuple [Lit (LitInt i'l), Lit (LitInt i'r)])
  = Right $ Lit (LitInt (i'l + i'r))
apply'operator "#*" (Tuple [Lit (LitInt i'l), Lit (LitInt i'r)])
  = Right $ Lit (LitInt (i'l * i'r))
apply'operator "#-" (Tuple [Lit (LitInt i'l), Lit (LitInt i'r)])
  = Right $ Lit (LitInt (i'l - i'r))
apply'operator "#/" (Tuple [Lit (LitInt i'l), Lit (LitInt i'r)])
  = Right $ Lit (LitInt (i'l `div` i'r))
apply'operator "#++" (Tuple [Lit (LitString s'l), Lit (LitString s'r)])
  = Right $ Lit (LitString (s'l ++ s'r))
apply'operator "#:" (Tuple [Lit (LitChar ch'l), Lit (LitString s'r)])
  = Right $ Lit (LitString (ch'l : s'r))
apply'operator "#;" (Tuple [Lit (LitChar ch'l), Lit (LitString s'r)])
  = Right $ Lit (LitString (s'r ++ [ch'l]))
apply'operator "#fst" (Tuple [f, s])
  = Right $ f
apply'operator "#snd" (Tuple [f, s])
  = Right $ s
apply'operator name expr
  = Left $ BadOperatorApplication name expr