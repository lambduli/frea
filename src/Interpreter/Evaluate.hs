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
force'val (Val.Thunk force'f env) = do
  result <- force'f env
  case result of
    Left err -> return $ Left err
    Right val -> force'val val
force'val val = return $ Right val


force :: Expression -> Env -> State Val.Memory (Either EvaluationError Val.Value)
force expr env = do
  result <- evaluate expr env
  case result of
      Right (Val.Thunk force'f env) -> do
        result <- force'f env
        case result of
          Right val -> force'val val
          Left err -> return $ Left err
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
            Nothing -> return $ Left $ Unexpected $ "V pameti sem nenasel " ++ name
        Nothing -> return $ Left $ UnboundVar name -- can't really happen thanks to the type system

    Op name ->
      return $ Right $ Val.Op name

    Lit lit ->
      return $ Right $ Val.Lit lit

    Tuple exprs ->
      return $ Right $ Val.Tuple $ map (\ expr -> Val.Thunk (\ env -> evaluate expr env) env) exprs

    Let name val expr ->
      return $ Right $ Val.Thunk (\ env -> evaluate (App (Lam name expr) val) env) env

    Lam par body ->
      return $ Right $ Val.Lam par body env


    App (Lam par body) right ->
      let right'val = Val.Thunk (\ env -> force right env) env
      in return $ Right $ Val.Thunk
        (\ e'map -> do
          mem <- get
          let addr = Addr $ Map.size mem
              env' = Map.insert par addr e'map
              mem' = Map.insert addr right'val mem
          put mem'
          evaluate body env') env

    App (Op op) right -> do
      res <- force right env
      case res of
        Right r'val -> apply'operator op r'val env
        Left err -> return $ Left err

    App left right ->
      return $ Right $ Val.Thunk (\ env -> do
        res <- force left env
        case res of
          Left err -> return $ Left err

          Right (Val.Lam par body env') -> do
            mem <- get
            let addr = Addr $ Map.size mem
                env'' = Map.insert par addr env'
                right'val = Val.Thunk (\ env -> force right env) env
                mem' = Map.insert addr right'val mem
            put mem'
            force body env''

          Right (Val.Op name) ->
            force (App (Op name) right) env) env

    If cond' then' else' ->
      return $ Right $ Val.Thunk (\ env -> do
        res <- force cond' env
        case res of
          Left err -> return $ Left err
          Right (Val.Data con'name []) -> -- wiring the Bool into the type checker
            if con'name == "True" then evaluate then' env else evaluate else' env) env

    Fix expr ->
      return $ Right $ Val.Thunk (\ env -> evaluate (App expr $ Fix expr) env) env

    Intro name exprs -> do
      mem <- get
      let values = map (\ expr -> Val.Thunk (\ env -> evaluate expr env) env) exprs
      return $ Right $ Val.Thunk (\ env -> return . Right $ Val.Data name values) env
      
      -- values <- mapM (\ expr -> force expr env) exprs
      -- case sequence values of
      --   Left wrong -> return $ Left wrong
      --   Right vals -> return $ Right $ Val.Thunk (\ env -> return . Right $ Val.Data name vals) env

    Elim constructors value'to'elim destructors ->
      return $ Right $ Val.Thunk (\ env -> do
        res <- force value'to'elim env
        case res of
          Right (Val.Data tag arguments) -> do
            let
              [(_, destr)] = filter (\ (ConDecl name _, _) -> tag == name) (zip constructors destructors)
              -- app = foldl App destr arguments
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
          -- something -> trace ("something  " ++ Val.present mem something) $ Right something
      ) env
        

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

apply'operator "#&&" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  case res'l of
    Right b -- wiring the Bool into the typechecker
        | Val.from'val'bool b -> return $ Right val'r
        | otherwise  -> return $ Right $ Val.to'val'bool False

apply'operator "#||" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  case res'l of
    Right b -- wiring the Bool into the typechecker
      | Val.from'val'bool b -> return $ Right val'r
      | otherwise  -> return $ Right $ Val.to'val'bool True

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

apply'operator "#+." (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l + d'r))

apply'operator "#*" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r))) ->
      return $ Right $ Val.Lit (LitInt (i'l * i'r))

apply'operator "#*." (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l * d'r))

apply'operator "#-" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r))) ->
      return $ Right $ Val.Lit (LitInt (i'l - i'r))

apply'operator "#-." (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l - d'r))

apply'operator "#div" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt 0))) ->
      return $ Left $ DivisionByZero i'l
    (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r))) ->
      return $ Right $ Val.Lit (LitInt (i'l `div` i'r))

apply'operator "#/" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble 0))) ->
      return $ Left $ DivisionByZero 0
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l / d'r))

apply'operator "#fst" (Val.Tuple [f, s]) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right f) env

apply'operator "#snd" (Val.Tuple [f, s]) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right s) env

apply'operator "#show" val env = do
  res <- force'val val
  case res of
    Left err -> return $ Left err
    Right val -> return $ Right $ Val.str'to'value $ show val -- wiring the String into the compiler

apply'operator name expr env
  = return $ Left $ BadOperatorApplication name expr
