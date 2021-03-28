module Interpreter.Evaluate where

import Data.Either
import Data.List
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map


import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration
import Compiler.Syntax.Literal
import Interpreter.Value (EvaluationError(..), Env(..))
import qualified Interpreter.Value as Val
import Interpreter.Address
import Debug.Trace



force'val :: Val.Value -> Val.Memory -> Either EvaluationError Val.Value
force'val (Val.Thunk force'f env) mem = do
  val <- force'f env mem
  force'val val mem
force'val val mem = Right val


force :: Expression -> Env -> Val.Memory -> Either EvaluationError Val.Value
force expr env mem
  = case evaluate expr env mem of
      Right (Val.Thunk force'f env) -> do
        val <- force'f env mem
        force'val val mem
      Right val -> Right val
      Left err -> Left err


evaluate :: Expression -> Env -> Val.Memory -> Either EvaluationError Val.Value
evaluate expr env@(Env e'map) mem = case expr of
  Var name
    | Just addr <- e'map !? name, Just val <- mem !? addr -> trace ("promenna jmenem " ++ name ++ "  a ta je na adrese " ++ show (e'map Map.! name) ) $ Right val

    | otherwise -> Left $ UnboundVar name -- can't really happen thanks to the type system

  Op name ->
    Right $ Val.Op name

  Lit lit ->
    Right $ Val.Lit lit

  Tuple exprs ->
    Right $ Val.Tuple $ map (\ expr -> Val.Thunk (\ env mem -> evaluate expr env mem) env) exprs

  List exprs ->
    Right $ Val.List $ map (\ expr -> Val.Thunk (\ env mem -> evaluate expr env mem) env) exprs

  Let name val expr ->
    Right $ Val.Thunk (\ env mem -> evaluate (App (Lam name expr) val) env mem) env

  Lam par body ->
    Right $ Val.Lam par body env


  App (Lam par body) right ->
    let right'val = Val.Thunk (\ env mem -> force right env mem) env
    in Right $ Val.Thunk
      (\ (Val.Env e'map) mem ->
        let addr = Addr $ Map.size $ trace ("pridavam do memu " ++ Val.present mem right'val) mem
            env' = Val.Env $ Map.insert par addr e'map
            mem' = Map.insert addr right'val $ trace ("env vypada takhle " ++ show env') mem
        in evaluate body env' mem') env

  App (Op op) right ->
    case force right env mem of
      Right r'val -> apply'operator op r'val env mem
      Left err -> Left err

  App left right ->
    Right $ Val.Thunk (\ env mem ->
      case force left env mem of
        Left err -> Left err

        Right (Val.Lam par body env'@(Val.Env bs')) ->
          let addr = Addr $ Map.size mem
              env'' = Val.Env $ Map.insert par addr bs'
              right'val = Val.Thunk (\ env mem -> force right env mem) env
              mem' = Map.insert addr right'val mem
          in  force body env'' mem'
        Right (Val.Op name) ->
          force (App (Op name) right) env mem) env

  If cond' then' else' ->
    Right $ Val.Thunk (\ env mem ->
      case force cond' env mem of
        Left err -> Left err
        Right (Val.Lit (LitBool b)) ->
          if b then evaluate then' env mem else evaluate else' env mem) env

  Fix expr ->
    Right $ Val.Thunk (\ env mem ->
      evaluate (App expr $ Fix expr) env mem) env

  Intro name exprs ->
    let values = map (\ expr -> Val.Thunk (\ env mem -> evaluate expr env mem) env) exprs
    in Right $ Val.Thunk (\ env mem -> Right $ Val.Data name values) env

  Elim constructors value'to'elim destructors ->
    Right $ Val.Thunk (\ env mem -> do
      let (Val.Env env') = env
      val <- force value'to'elim env $ (trace $ "celej env' " ++ show env') mem
      case val of
        Val.Data tag arguments ->
          let
            [(_, destr)] = filter (\ (ConDecl name _, _) -> tag == name) (zip constructors destructors)
            -- app = foldl App destr arguments
          in case force destr env mem of
            Left err -> Left err

            Right val | [] <- arguments ->
              Right val

            Right lam@(Val.Lam par body (Env env')) ->
              apply'closure arguments lam mem

            Right (Val.Op op) | [right] <- arguments ->
              case force'val right mem of
                Left err -> Left err
                Right r'val -> apply'operator op r'val env mem
        something -> trace ("something  " ++ Val.present mem something) $ Right something
    ) env
        


apply'closure :: [Val.Value] -> Val.Value  -> Val.Memory -> Either EvaluationError Val.Value
apply'closure [] val _ = Right val
apply'closure (val : vals) (Val.Lam par body (Env env)) mem
  = let addr = Addr $ Map.size mem
        env' = Val.Env $ Map.insert par addr env
        mem' = Map.insert addr val mem
    in
      case force body env' mem' of
        Left err -> Left err
        Right body'val -> apply'closure vals body'val mem'


apply'operator :: String -> Val.Value -> Env -> Val.Memory -> Either EvaluationError Val.Value
apply'operator "#=" (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit lit'l), Right (Val.Lit lit'r))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitBool (lit'l == lit'r))

apply'operator "#&&" (Val.Tuple [val'l, val'r]) env mem
  = case force'val val'l mem of
      Right (Val.Lit (LitBool b))
        | b -> Right val'r
        | otherwise  -> Right $ Val.Lit $ LitBool False

apply'operator "#||" (Val.Tuple [val'l, val'r]) env mem
  = case force'val val'l mem of
      Right (Val.Lit (LitBool b))
        | not b -> Right val'r
        | otherwise  -> Right $ Val.Lit $ LitBool True

apply'operator "#<" (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit lit'l), Right (Val.Lit lit'r))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitBool (lit'l < lit'r))

apply'operator "#>" (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit lit'l), Right (Val.Lit lit'r))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitBool (lit'l > lit'r))

apply'operator "#+" (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitInt (i'l + i'r))

apply'operator "#+." (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitDouble (d'l + d'r))

apply'operator "#*" (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitInt (i'l * i'r))

apply'operator "#*." (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitDouble (d'l * d'r))

apply'operator "#-" (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitInt (i'l - i'r))

apply'operator "#-." (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitDouble (d'l - d'r))

apply'operator "#div" (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitInt (i'l `div` i'r))
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt 0)))
      <- (force'val val'l mem, force'val val'r mem)
        = Left $ DivisionByZero i'l

apply'operator "#/" (Val.Tuple [val'l, val'r]) env mem
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
      <- (force'val val'l mem, force'val val'r mem)
        = Right $ Val.Lit (LitDouble (d'l / d'r))
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble 0)))
      <- (force'val val'l mem, force'val val'r mem)
        = Left $ DivisionByZero 0

apply'operator "#++" (Val.Tuple [val'l, val'r]) env mem
  = Right $ Val.Thunk (\ env mem ->
      case (force'val val'l mem, force'val val'r mem) of
        (Right (Val.List exprs'left), Right (Val.List exprs'right)) ->
          Right $ Val.List $ exprs'left ++ exprs'right
        (Right (Val.Lit (LitString str'left)), Right (Val.Lit (LitString str'right))) ->
          Right $ Val.Lit $ LitString $ str'left ++ str'right
        (Left err, _) -> Left err
        (_, Left err) -> Left err) env

-- when the val'r is not a List, but an infinite expression instead
-- forcing the val'r makes the program to run forever 
apply'operator "#:" (Val.Tuple [val'l, val'r]) env mem
  = Right $ Val.Thunk (\ env mem ->
      case force'val val'r mem of
        Right (Val.Lit (LitString str))
          | Right (Val.Lit (LitChar ch)) <- force'val val'l mem ->
            Right $ Val.Lit $ LitString $ ch : str
          | Left err <- force'val val'l mem -> Left err
          | otherwise -> Left $ BadOperatorApplication "#:" val'l

        Right (Val.List exprs) -> Right $ Val.List $ val'l : exprs

        Left err -> Left err) env

apply'operator "#head" (Val.List []) env mem
  = Right $ Val.Thunk (\ _ _ -> Left NilHeadException) env

apply'operator "#head" (Val.Lit (LitString "")) env mem
  = Right $ Val.Thunk (\ _ _ -> Left EmptyStringException) env

apply'operator "#head" (Val.List (e : es)) env mem
  = Right $ Val.Thunk (\ _ _ -> Right e) env

apply'operator "#head" (Val.Lit (LitString (e : es))) env mem
  = Right $ Val.Thunk (\ _ _ -> Right $ Val.Lit $ LitChar e) env

apply'operator "#tail" (Val.List []) env mem
  = Right $ Val.Thunk (\ _ _ -> Left NilTailException) env

apply'operator "#tail" (Val.Lit (LitString "")) env mem
  = Right $ Val.Thunk (\ _ _ -> Left EmptyStringException) env

apply'operator "#tail" (Val.List (e : es)) env mem
  = Right $ Val.Thunk (\ _ _ -> Right $ Val.List es) env

apply'operator "#tail" (Val.Lit (LitString (e : es))) env mem
  = Right $ Val.Thunk (\ _ _ -> Right $ Val.Lit $ LitString es) env

apply'operator "#nil?" (Val.List []) env mem
  = Right $ Val.Thunk (\ _ _ -> Right $ Val.Lit $ LitBool True) env

apply'operator "#nil?" (Val.Lit (LitString "")) env mem
  = Right $ Val.Thunk (\ _ _ -> Right $ Val.Lit $ LitBool True) env

apply'operator "#nil?" (Val.List (e : es)) env mem
  = Right $ Val.Thunk (\ _ _ -> Right $ Val.Lit $ LitBool False) env

apply'operator "#nil?" (Val.Lit (LitString (e : es))) env mem
  = Right $ Val.Thunk (\ _ _ -> Right $ Val.Lit $ LitBool False) env

apply'operator "#fst" (Val.Tuple [f, s]) env mem
  = Right $ Val.Thunk (\ _ _ -> Right f) env

apply'operator "#snd" (Val.Tuple [f, s]) env mem
  = Right $ Val.Thunk (\ _ _ -> Right s) env

apply'operator name expr env mem
  = Left $ BadOperatorApplication name expr
