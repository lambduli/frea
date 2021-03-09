module Interpreter.Evaluate where

import qualified Data.Map as Map
import Data.Either
import Data.List

import Compiler.Syntax.Expression
import Compiler.Syntax.Literal
import Interpreter.Error
import qualified Interpreter.Value as Val


substitute :: Expression -> String -> Expression -> Expression
substitute (Var name) var replacement
  | name == var = replacement
  | otherwise = Var name
substitute (Op o) _ _ = Op o
substitute (Lit l) _ _ = Lit l
substitute (Lam par body) var replacement
  | par /= var = Lam par $ substitute body var replacement
  | otherwise = Lam par body
substitute (App left right) var replacement
  = App (substitute left var replacement) (substitute right var replacement)
substitute (Tuple exprs) var replacement
  = Tuple $ map (\ e -> substitute e var replacement) exprs
substitute (List exprs) var replacement
  = List $ map (\ e -> substitute e var replacement) exprs
substitute (If cond' then' else') var replacement
  = If (substitute cond' var replacement) (substitute then' var replacement) (substitute else' var replacement)
substitute (Let name val expr) var replacement
  | name /= var = Let name (substitute val var replacement) (substitute expr var replacement)
  | otherwise = Let name val expr
substitute (Fix expr) var replacement
  = Fix $ substitute expr var replacement
--   = Typed type' (substitute expr var replacement)


evaluate :: Expression -> Val.Env -> Either EvaluationError Val.Value
evaluate expr env@(Val.Env bs) = case expr of
  Var name
    | Just (expr, env') <- lookup name bs -> evaluate expr env'
    | otherwise -> Left $ UnboundVar name

  Op name
    | Just (expr, env') <- lookup name bs -> evaluate expr env'
    | otherwise -> Right $ Val.Op name

  Lit lit ->
    Right $ Val.Lit lit

  Tuple exprs ->
    let
      eiths = map (`evaluate` env) exprs
      may'err = find isLeft eiths
    in case may'err of
      Nothing ->
        let
          from'right (Right x) = x
          vals = map from'right eiths
        in Right $ Val.Tuple vals
      Just err -> err

  List exprs ->
    let
      eiths = map (`evaluate` env) exprs
      may'err = find isLeft eiths
    in case may'err of
      Nothing ->
        let
          from'right (Right x) = x
          vals = map from'right eiths
        in Right $ Val.List vals
      Just err -> err

  Let name val expr ->
    evaluate (App (Lam name expr) val) env

  Lam par body ->
    Right $ Val.Lam par body env

  App (Lam par body) right ->
    evaluate body $ Val.Env ((par, (right, env)) : bs)
    -- case evaluate right env of
    --   Right r'val -> 
    --   Left err -> Left err

  App (Op op) right ->
    case evaluate right env of
      Right r'val -> apply'operator op r'val env
      Left err -> Left err

  App left right ->
    case evaluate left env of
      Left err -> Left err
      Right (Val.Lam par body (Val.Env bs')) ->
        evaluate body $ Val.Env ((par, (right, env)) : bs')
        -- case evaluate right env of
        --   Right r'val -> evaluate body $ Val.Env ((par, r'val) : env)
        --   Left err -> Left err

  If cond' then' else' ->
    case evaluate cond' env of
      Left err -> Left err
      Right (Val.Lit (LitBool b)) ->
        if b then evaluate then' env else evaluate else' env

  Fix expr ->
    evaluate (App expr $ Fix expr) env


apply'operator :: String -> Val.Value -> Val.Env -> Either EvaluationError Val.Value
apply'operator "#=" (Val.Tuple [Val.Lit lit'l, Val.Lit lit'r]) env
  = Right $ Val.Lit (LitBool (lit'l == lit'r))
apply'operator "#<" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  = Right $ Val.Lit (LitBool (i'l < i'r))
apply'operator "#>" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  = Right $ Val.Lit (LitBool (i'l > i'r))
apply'operator "#+" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  = Right $ Val.Lit (LitInt (i'l + i'r))
apply'operator "#*" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  = Right $ Val.Lit (LitInt (i'l * i'r))
apply'operator "#-" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  = Right $ Val.Lit (LitInt (i'l - i'r))
apply'operator "#/" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt 0)]) env
  = Left $ DivisionByZero i'l
apply'operator "#/" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  = Right $ Val.Lit (LitInt (i'l `div` i'r))
apply'operator "#." (Val.Tuple [Val.Lit (LitString s'l), Val.Lit (LitString s'r)]) env
  = Right $ Val.Lit (LitString (s'l ++ s'r))
apply'operator "#++" (Val.Tuple [Val.List exprs'left, Val.List exprs'right]) env
  = Right $ Val.List $ exprs'left ++ exprs'right
apply'operator "#;" (Val.Tuple [Val.Lit (LitChar ch'l), Val.Lit (LitString s'r)]) env
  = Right $ Val.Lit (LitString (ch'l : s'r))
apply'operator "#:" (Val.Tuple [expr, Val.List exprs]) env
  = Right $ Val.List $ expr : exprs 
apply'operator "#!!" (Val.Tuple [Val.Lit (LitInt i), Val.List exprs]) env
  | i < 0 || i >= length exprs = Left $ IndexOutOfBound i
  | otherwise = Right $ exprs !! i
apply'operator "#head" (Val.List []) env
  = Left NilHeadException
apply'operator "#head" (Val.List (e : es)) env
  = Right e
apply'operator "#tail" (Val.List []) env
  = Left NilTailException
apply'operator "#tail" (Val.List (e : es)) env
  = Right $ Val.List es
apply'operator "#nil?" (Val.List []) env
  = Right $ Val.Lit $ LitBool True
apply'operator "#nil?" (Val.List (e : es)) env
  = Right $ Val.Lit $ LitBool False
apply'operator "#fst" (Val.Tuple [f, s]) env
  = Right f
apply'operator "#snd" (Val.Tuple [f, s]) env
  = Right s
apply'operator name expr env
  = Left $ BadOperatorApplication name expr
