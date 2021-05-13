module Interpreter.Evaluate where

import Data.Either
import Data.List
import Data.Map.Strict ((!?), (!))
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration
import Compiler.Syntax.Literal
import Interpreter.Value (EvaluationError(..), Env(..))
import qualified Interpreter.Value as Val
import Interpreter.Address
import Debug.Trace



force'val :: Val.Value -> State Val.Memory (Either EvaluationError Val.Value)
force'val (Val.Thunk force'f env addr) = do
  result <- force'f env
  case result of
    Left err -> return $ Left err
    Right val -> do
      mem <- get
      let mem' = Map.insert addr val mem
      put mem'
      force'val val
force'val val = return $ Right val


force :: Expression -> Env -> State Val.Memory (Either EvaluationError Val.Value)
force expr env = do
  result <- evaluate expr env
  case result of
      Right (Val.Thunk force'f env addr) -> do
        result <- force'f env
        case result of
          Left err -> return $ Left err
          Right val -> do
            mem <- get
            let mem' = Map.insert addr val mem
            put mem'
            force'val val
      Right val -> return $ Right val
      Left err -> return $ Left err


evaluate :: Expression -> Env -> State Val.Memory (Either EvaluationError Val.Value)
evaluate expr env =
  case expr of
    Var name -> do
      mem <- get
      case env !? name of
        Just addr ->
          case mem !? addr of
            Just val -> return $ Right val
            Nothing -> return $ Left $ Unexpected $ "Value of " ++ name ++ " not found in the memory for some reason."
        Nothing -> return $ Left $ UnboundVar name -- can't really happen thanks to the type system

    Op name ->
      return $ Right $ Val.Op name

    Lit lit ->
      return $ Right $ Val.Lit lit

    Tuple exprs ->
       return $ Right $ Val.Tuple $ map (\ expr -> Val.Thunk (\ env -> evaluate expr env) env (Addr (-1))) exprs

    Let bind'pairs expr ->
      -- TODO: do the same thing as for global bindings
      -- first collect them all to the env
      -- then store them all in the memory
      -- then set new memory and evaluate the expr with the new env
      evaluate (App (Lam name expr) val) env

    Lam par body ->
      return $ Right $ Val.Lam par body env


    App (Lam par body) right -> do
      mem <- get
      let addr = Addr $ Map.size mem
          right'val = Val.Thunk (\ env -> force right env) env addr
          env' = Map.insert par addr env
          mem' = Map.insert addr right'val mem
      put mem'
      evaluate body env'

    App (Op op) right -> do
      res <- force right env
      case res of
        Right r'val -> apply'operator op r'val env
        Left err -> return $ Left err

    App left right -> do
      res <- force left env
      case res of
        Left err -> return $ Left err

        Right (Val.Lam par body env') -> do
          mem <- get
          let addr = Addr $ Map.size mem
              env'' = Map.insert par addr env'
              right'val = Val.Thunk (\ env -> force right env) env addr
              mem' = Map.insert addr right'val mem
          put mem'
          evaluate body env''

        Right (Val.Op name) ->
            evaluate (App (Op name) right) env

    If cond' then' else' -> do
      res <- force cond' env
      case res of
        Left err -> return $ Left err

        Right (Val.Data con'name []) -- wiring the Bool into the type checker
          | con'name == "True" -> evaluate then' env
          | otherwise -> evaluate else' env

    Fix expr ->
      evaluate (App expr $ Fix expr) env

    Intro name exprs -> do
      let values = map (\ expr -> Val.Thunk (\ env -> evaluate expr env) env (Addr (-1))) exprs
      return . Right $ Val.Data name values

    Elim constructors value'to'elim destructors -> do
      res <- force value'to'elim env
      case res of
        Right (Val.Data tag arguments) -> do
          let
            [(_, destr)] = filter (\ (ConDecl name _, _) -> tag == name) (zip constructors destructors)
          res <- force destr env
          case res of
            Left err -> return $ Left err

            Right val | [] <- arguments ->
              return $ Right val

            Right lam@(Val.Lam par body _) ->
              apply'closure arguments lam

            Right (Val.Op op) | [right] <- arguments -> do
              res <- force'val right
              case res of
                Left err -> return $ Left err
                Right r'val -> apply'operator op r'val env

        Left err -> return $ Left err

    Ann _ expr -> evaluate expr env
        

apply'closure :: [Val.Value] -> Val.Value -> State Val.Memory (Either EvaluationError Val.Value)
apply'closure [] val = return $ Right val
apply'closure (val : vals) (Val.Lam par body env) = do
  mem <- get
  let addr = Addr $ Map.size mem
      env' = Map.insert par addr env
      mem' = Map.insert addr val mem
  put mem'
  res <- force body env'
  case res of
    Left err -> return $ Left err
    Right body'val -> apply'closure vals body'val


apply'operator :: String -> Val.Value -> Env -> State Val.Memory (Either EvaluationError Val.Value)
apply'operator "#=" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit lit'l), Right (Val.Lit lit'r)) ->
      return $ Right $ Val.to'val'bool (lit'l == lit'r) -- wiring the Bool into the typechecker
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#<" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit lit'l), Right (Val.Lit lit'r)) -> -- wiring the Bool into the typechecker
      return $ Right $ Val.to'val'bool (lit'l < lit'r)
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#>" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit lit'l), Right (Val.Lit lit'r)) -> -- wiring the Bool into the typechecker
      return $ Right $ Val.to'val'bool (lit'l > lit'r)
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#+" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r))) ->
      return $ Right $ Val.Lit (LitInt (i'l + i'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err


apply'operator "#+." (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l + d'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#*" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r))) ->
      return $ Right $ Val.Lit (LitInt (i'l * i'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#*." (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l * d'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#-" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r))) ->
      return $ Right $ Val.Lit (LitInt (i'l - i'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#-." (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l - d'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#div" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt 0))) ->
      return $ Left $ DivisionByZero i'l
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r))) ->
      return $ Right $ Val.Lit (LitInt (i'l `div` i'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#/" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble 0))) ->
      return $ Left $ DivisionByZero 0
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l / d'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#fst" (Val.Tuple [f, s]) env
  = return $ Right f

apply'operator "#snd" (Val.Tuple [f, s]) env
  = return $ Right s

apply'operator "#show" val env = do
  res <- force'val val
  case res of
    Left err -> return $ Left err
    Right val -> return $ Right $ Val.str'to'value $ show val -- wiring the String into the compiler

apply'operator "#debug" val env = do
  res <- force'val val
  case res of
    Left err -> return $ Left err
    Right val ->
      let v = trace ("@debug:  `" ++ show val ++ "`") val
      in return $ Right v

apply'operator name expr env
  = return $ Left $ BadOperatorApplication name expr
