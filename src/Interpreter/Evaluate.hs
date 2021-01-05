module Interpreter.Evaluate where


import qualified Data.Map as Map
import Data.Either
import Data.List

import Compiler.Syntax.Expression
import Compiler.Syntax.Literal


type Env = Map.Map String Expression

data EvaluationError
  = UnboundVar String
  | WrongNegation Expression
  | ApplicationError Expression Expression -- Function Argument
  | BadOperatorApplication String Expression
  deriving (Show)


substitute :: Expression -> String -> Expression -> Expression
substitute (Var name) var replacement
  | name == var = replacement
  | otherwise = Var name
substitute (Op o) _ _ = Op o
substitute (Con c) _ _ = Con c
substitute (Lit l) _ _ = Lit l
substitute (Lam par body) var replacement
  | par /= var = Lam par $ substitute body var replacement
  | otherwise = Lam par body
substitute (App left right) var replacement
  = App (substitute left var replacement) (substitute right var replacement)
substitute (Tuple exprs) var replacement
  = Tuple $ map (\ e -> substitute e var replacement) exprs
substitute (NegApp expr) var replacement
  = NegApp $ substitute expr var replacement
substitute (If cond' then' else') var replacement
  = If (substitute cond' var replacement) (substitute then' var replacement) (substitute else' var replacement)
substitute (Let name val expr) var replacement
  | name /= var = Let name (substitute val var replacement) (substitute expr var replacement)
  | otherwise = Let name val expr
substitute (Typed type' expr) var replacement
  = Typed type' (substitute expr var replacement)


eval :: Expression -> Either EvaluationError Expression
eval expr = evaluate expr

evaluate :: Expression -> Either EvaluationError Expression
evaluate expr = case expr of
  Var name -> Right $ Var name
  Op name -> Right $ Op name
  -- Con name -> Right $ Con name
  Lit lit -> Right $ Lit lit
  Tuple exprs ->
    let
      eiths = map evaluate exprs
      may'err = find isLeft eiths
    in case may'err of
      Nothing ->
        let
          from'right (Right x) = x
          vals = map from'right eiths
        in Right $ Tuple vals
      Just err -> err
    -- Tuple $ map (\ e -> evaluate e env) exprs
  Let name val expr ->
    evaluate $ App (Lam name expr) val
  Lam arg body -> Right $ Lam arg body
  App (Lam arg body) right -> case evaluate right of
    Left err -> Left err
    Right val -> evaluate $ substitute body arg val
  App (Op op) right -> case evaluate right of
    Left err -> Left err
    Right val -> apply'operator op val
  App left right -> case evaluate left of
    Left err -> Left err
    Right fn' -> evaluate $ App fn' right    
    -- App (Con arg) right -> 
  NegApp expr -> case evaluate expr of
    Right (Lit (LitInt i)) -> Right $ Lit (LitInt (-i))
    Right (Lit (LitDouble d)) -> Right $ Lit (LitDouble (-d))
    Left err -> Left err
    _ -> Left $ WrongNegation expr
  If cond' then' else' ->
    let
      cond'evld = evaluate cond'
      then'evld = evaluate then'
      else'evld = evaluate else'
    in case find isLeft [cond'evld, then'evld, else'evld] of
      Just err -> err
      Nothing ->
        let (Right (Lit (LitBool b))) = cond'evld
        in if b then then'evld else else'evld


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