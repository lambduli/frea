module Interpreter.Print where

import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map.Strict as Map

import Compiler.Syntax.Literal

import Interpreter.Value
import Interpreter.Evaluate


print :: State Memory (Either EvaluationError Value) -> State Memory (Either EvaluationError String)
print state = do
  res <- state
  case res of
    Left err -> return $ Left err

    Right val -> print' val

    where
      print' :: Value -> State Memory (Either EvaluationError String)
      print' (Op op) = return $ Right op
      print' (Lit lit) = return $ Right $ show lit
      print' (Lam par expr env) = return $ Right "<lambda>"
      print' (Tuple vals) = do
        printed'vals <- mapM print' vals
        case sequence printed'vals of
          Left err -> return $ Left err
          Right vals -> return $ Right $ "(" ++ intercalate ", " vals ++ ")"
      print' t@(Thunk force'f env addr) = do
        val <- force'val t
        case val of
          Left err -> return $ Left err
          Right val' -> do
            mem <- get
            let mem' = Map.insert addr val' mem
            put mem'
            print' val'

      print' (Data con'tag []) = return $ Right con'tag
      
      {-
        This is really ugly hack to achieve printing strings as strings and lists as lists
      -}

      print' (Data ":" [head, tail]) = do
        h'fced <- force'val head
        tail'fced <- force'val tail
        case h'fced of
          Left err -> return $ Left err
          Right (Lit (LitChar ch)) ->
            case tail'fced of
              Left err -> return $ Left err
              Right tail'v -> do -- je to teda nejakej dalsi pair char a string
              -- takze to necham print' nout a concatnu je
                rest <- print'list tail'v
                case rest of
                  Left err -> return $ Left err
                  Right str -> return $ Right $ '"' : ch : str ++ "\""
            -- ted vim ze to bude string
            -- takze muzu jit pro zbytek a vzdycky evaluovat
          Right h'v -> do -- ted je to pripad, kdyz to sice je list, ale ne charu
          -- takze to musim osetrit normalne
            str <- print' h'v
            case str of
              Left err -> return $ Left err
              Right str' -> do
                let prefix = "["
                case tail'fced of
                  Left err -> return $ Left err
                  Right (Data "[]" []) -> -- tohle je teda konec listu
                    return $ Right $ prefix ++ str' ++ "]"
                  Right tail'v -> do -- je to teda nejakej dalsi pari "val" a "list"
                  -- takze to necham print' nout a concatnu je
                    rest <- print'list tail'v
                    case rest of
                      Left err -> return $ Left err
                      Right sstr -> return $ Right $ prefix ++ str' ++ ", " ++ sstr

      print' (Data con'tag args) = do
        printed'vals <- mapM print' args
        case sequence printed'vals of
          Left err -> return $ Left err
          Right vals -> return $ Right $ "(" ++ con'tag ++ " " ++ unwords vals ++ ")"

      print'list (Data "[]" []) = return $ Right ""

      print'list (Data ":" [head, tail]) = do
        h'fced <- force'val head
        tail'fced <- force'val tail
        case h'fced of
          Left err -> return $ Left err
          Right (Lit (LitChar ch)) ->
            case tail'fced of
              Left err -> return $ Left err
              -- Right (Data "[]" []) -> -- tohle je teda konec stringu
              --   return $ Right [ch]
              Right tail'v -> do -- je to teda nejakej dalsi pair char a string
              -- takze to necham print' nout a concatnu je
                rest <- print'list tail'v
                case rest of
                  Left err -> return $ Left err
                  Right str -> return $ Right $ ch : str
            -- ted vim ze to bude string
            -- takze muzu jit pro zbytek a vzdycky evaluovat
          Right h'v -> do -- ted je to pripad, kdyz to sice je list, ale ne charu
          -- takze to musim osetrit normalne
            str <- print' h'v
            case str of
              Left err -> return $ Left err
              Right str' -> do
                case tail'fced of
                  Left err -> return $ Left err
                  Right (Data "[]" []) -> -- tohle je teda konec listu
                    return $ Right $ str' ++ "]"
                  Right tail'v -> do -- je to teda nejakej dalsi pari "val" a "list"
                  -- takze to necham print' nout a concatnu je
                    rest <- print'list tail'v
                    case rest of
                      Left err -> return $ Left err
                      Right sstr -> return $ Right $ str' ++ ", " ++ sstr

      print'list unknown = return $ Left $ Unexpected $ "Something unexpected happened: " ++ show unknown
