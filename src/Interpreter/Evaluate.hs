module Interpreter.Evaluate where

import qualified Data.Map as Map
import Data.Either
import Data.List

import Compiler.Syntax.Expression
import Compiler.Syntax.Literal
import Interpreter.Error



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
-- substitute (NegApp expr) var replacement
--   = NegApp $ substitute expr var replacement
substitute (List exprs) var replacement
  = List $ map (\ e -> substitute e var replacement) exprs
substitute (If cond' then' else') var replacement
  = If (substitute cond' var replacement) (substitute then' var replacement) (substitute else' var replacement)
substitute (Let name val expr) var replacement
  | name /= var = Let name (substitute val var replacement) (substitute expr var replacement)
  | otherwise = Let name val expr
substitute (Fix expr) var replacement
  = Fix $ substitute expr var replacement
-- substitute (Typed type' expr) var replacement
--   = Typed type' (substitute expr var replacement)



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
  List exprs ->
    let
      eiths = map evaluate exprs
      may'err = find isLeft eiths
    in case may'err of
      Nothing ->
        let
          from'right (Right x) = x
          vals = map from'right eiths
        in Right $ List vals
      Just err -> err
  Let name val expr ->
    evaluate $ App (Lam name expr) val
  Lam arg body -> Right $ Lam arg body
  App (Lam arg body) right -> evaluate $ substitute body arg right
    -- case evaluate right of
      -- Left err -> Left err
      -- Right val -> evaluate $ substitute body arg val
  App (Op op) right -> case evaluate right of
    Left err -> Left err
    Right val -> apply'operator op val
  App left right -> case evaluate left of
    Left err -> Left err
    Right fn' -> evaluate $ App fn' right    
    -- App (Con arg) right -> 
  -- NegApp expr -> case evaluate expr of
  --   Right (Lit (LitInt i)) -> Right $ Lit (LitInt (-i))
  --   Right (Lit (LitDouble d)) -> Right $ Lit (LitDouble (-d))
  --   Left err -> Left err
  --   _ -> Left $ WrongNegation expr
  If cond' then' else' -> case evaluate cond' of
    Left err -> Left err
    Right (Lit (LitBool b)) -> if b then evaluate then' else evaluate else'
  Fix expr ->
    evaluate $ App expr $ Fix expr


apply'operator :: String -> Expression -> Either EvaluationError Expression
apply'operator "#=" (Tuple [Lit lit'l, Lit lit'r])
  = Right $ Lit (LitBool (lit'l == lit'r))
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
apply'operator "#/" (Tuple [Lit (LitInt i'l), Lit (LitInt 0)])
  = Left $ DivisionByZero i'l
apply'operator "#/" (Tuple [Lit (LitInt i'l), Lit (LitInt i'r)])
  = Right $ Lit (LitInt (i'l `div` i'r))
apply'operator "#." (Tuple [Lit (LitString s'l), Lit (LitString s'r)])
  = Right $ Lit (LitString (s'l ++ s'r))
apply'operator "#++" (Tuple [List exprs'left, List exprs'right])
  = Right $ List $ exprs'left ++ exprs'right
apply'operator "#;" (Tuple [Lit (LitChar ch'l), Lit (LitString s'r)])
  = Right $ Lit (LitString (ch'l : s'r))
apply'operator "#:" (Tuple [expr, List exprs])
  = Right $ List $ expr : exprs
apply'operator "#!!" (Tuple [Lit (LitInt i), List exprs])
  | i < 0 || i >= (length exprs) = Left $ IndexOutOfBound i
  | otherwise = Right $ exprs !! i
apply'operator "#head" (List []) = Left $ NilHeadException
apply'operator "#head" (List (e : es)) = Right e
apply'operator "#tail" (List []) = Left $ NilTailException
apply'operator "#tail" (List (e : es)) = Right $ List es
apply'operator "#nil?" (List []) = Right $ Lit $ LitBool True
apply'operator "#nil?" (List (e : es)) = Right $ Lit $ LitBool False
apply'operator "#fst" (Tuple [f, s])
  = Right $ f
apply'operator "#snd" (Tuple [f, s])
  = Right $ s
apply'operator name expr
  = Left $ BadOperatorApplication name expr