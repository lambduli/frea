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
evaluate expr env@(Env e'map) =
  case expr of
    Var name -> do
      mem <- get
      case e'map !? name of
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

    List exprs ->
      return $ Right $ Val.List $ map (\ expr -> Val.Thunk (\ env -> evaluate expr env) env) exprs

    Let name val expr ->
      return $ Right $ Val.Thunk (\ env -> evaluate (App (Lam name expr) val) env) env

    Lam par body ->
      return $ Right $ Val.Lam par body env


    App (Lam par body) right ->
      let right'val = Val.Thunk (\ env -> force right env) env
      in return $ Right $ Val.Thunk
        (\ (Val.Env e'map) -> do
          mem <- get
          let addr = Addr $ Map.size mem
              env' = Val.Env $ Map.insert par addr e'map
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

          Right (Val.Lam par body env'@(Val.Env bs')) -> do
            mem <- get
            let addr = Addr $ Map.size mem
                env'' = Val.Env $ Map.insert par addr bs'
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
          Right (Val.Lit (LitBool b)) ->
            if b then evaluate then' env else evaluate else' env) env

    Fix expr ->
      return $ Right $ Val.Thunk (\ env -> evaluate (App expr $ Fix expr) env) env

    Intro name exprs -> do
      -- let values = map (\ expr -> Val.Thunk (\ env -> evaluate expr env) env) exprs
      values <- mapM (\ expr -> force expr env) exprs
      case sequence values of
        Left wrong -> return $ Left wrong
        Right vals -> return $ Right $ Val.Thunk (\ env -> return . Right $ Val.Data name vals) env

    Elim constructors value'to'elim destructors ->
      return $ Right $ Val.Thunk (\ env -> do
        let (Val.Env env') = env
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

              Right lam@(Val.Lam par body (Env env')) ->
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
apply'closure (val : vals) (Val.Lam par body (Env env)) = do
  mem <- get
  let addr = Addr $ Map.size mem
      env' = Val.Env $ Map.insert par addr env
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
    (Right (Val.Lit lit'l), Right (Val.Lit lit'r)) -> return $ Right $ Val.Lit (LitBool (lit'l == lit'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

apply'operator "#&&" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  case res'l of
    Right (Val.Lit (LitBool b))
        | b -> return $ Right val'r
        | otherwise  -> return $ Right $ Val.Lit $ LitBool False

apply'operator "#||" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  case res'l of
    Right (Val.Lit (LitBool b))
      | not b -> return $ Right val'r
      | otherwise  -> return $ Right $ Val.Lit $ LitBool True

apply'operator "#<" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit lit'l), Right (Val.Lit lit'r)) -> return $ Right $ Val.Lit (LitBool (lit'l < lit'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err
  -- | (Right (Val.Lit lit'l), Right (Val.Lit lit'r))
  --     <- (force'val val'l mem, force'val val'r mem)
  --       = Right $ Val.Lit (LitBool (lit'l < lit'r))

apply'operator "#>" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit lit'l), Right (Val.Lit lit'r)) -> return $ Right $ Val.Lit (LitBool (lit'l > lit'r))
    (Right _, Left err) -> return $ Left err
    (Left err, _) -> return $ Left err

  -- | (Right (Val.Lit lit'l), Right (Val.Lit lit'r))
  --     <- (force'val val'l mem, force'val val'r mem)
  --       = Right $ Val.Lit (LitBool (lit'l > lit'r))

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

  -- | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
  --     <- (force'val val'l mem, force'val val'r mem)
  --       = Right $ Val.Lit (LitInt (i'l `div` i'r))
  -- | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt 0)))
  --     <- (force'val val'l mem, force'val val'r mem)
  --       = Left $ DivisionByZero i'l

apply'operator "#/" (Val.Tuple [val'l, val'r]) env = do
  res'l <- force'val val'l
  res'r <- force'val val'r
  case (res'l, res'r) of
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble 0))) ->
      return $ Left $ DivisionByZero 0
    (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r))) ->
      return $ Right $ Val.Lit (LitDouble (d'l / d'r))

  -- | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
  --     <- (force'val val'l mem, force'val val'r mem)
  --       = Right $ Val.Lit (LitDouble (d'l / d'r))
  -- | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble 0)))
  --     <- (force'val val'l mem, force'val val'r mem)
  --       = Left $ DivisionByZero 0

apply'operator "#++" (Val.Tuple [val'l, val'r]) env =
    return $ Right $ Val.Thunk (\ env -> do
      res'l <- force'val val'l
      res'r <- force'val val'r
      case (res'l, res'r) of
        (Right (Val.List exprs'left), Right (Val.List exprs'right)) ->
          return $ Right $ Val.List $ exprs'left ++ exprs'right
        (Right (Val.Lit (LitString str'left)), Right (Val.Lit (LitString str'right))) ->
          return $ Right $ Val.Lit $ LitString $ str'left ++ str'right
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
    ) env

-- when the val'r is not a List, but an infinite expression instead
-- forcing the val'r makes the program to run forever 
apply'operator "#:" (Val.Tuple [val'l, val'r]) env = do
    return $ Right $ Val.Thunk (\ env -> do
      res'r <- force'val val'r
      case res'r of
        Right (Val.Lit (LitString str)) -> do
          res'l <- force'val val'l
          case res'l of
            Right (Val.Lit (LitChar ch)) ->
              return $ Right $ Val.Lit $ LitString $ ch : str
            Left err -> return $ Left err
          
        Right (Val.List exprs) -> return $ Right $ Val.List $ val'l : exprs
        
        _ -> return $ Left $ BadOperatorApplication "#:" val'l) env

apply'operator "#head" (Val.List []) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Left NilHeadException) env

apply'operator "#head" (Val.Lit (LitString "")) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Left EmptyStringException) env

apply'operator "#head" (Val.List (e : es)) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right e) env

apply'operator "#head" (Val.Lit (LitString (e : es))) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right $ Val.Lit $ LitChar e) env

apply'operator "#tail" (Val.List []) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Left NilTailException) env

apply'operator "#tail" (Val.Lit (LitString "")) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Left EmptyStringException) env

apply'operator "#tail" (Val.List (e : es)) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right $ Val.List es) env

apply'operator "#tail" (Val.Lit (LitString (e : es))) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right $ Val.Lit $ LitString es) env

apply'operator "#nil?" (Val.List []) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right $ Val.Lit $ LitBool True) env

apply'operator "#nil?" (Val.Lit (LitString "")) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right $ Val.Lit $ LitBool True) env

apply'operator "#nil?" (Val.List (e : es)) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right $ Val.Lit $ LitBool False) env

apply'operator "#nil?" (Val.Lit (LitString (e : es))) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right $ Val.Lit $ LitBool False) env

apply'operator "#fst" (Val.Tuple [f, s]) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right f) env

apply'operator "#snd" (Val.Tuple [f, s]) env
  = return $ Right $ Val.Thunk (\ _ -> return $ Right s) env

apply'operator name expr env
  = return $ Left $ BadOperatorApplication name expr
