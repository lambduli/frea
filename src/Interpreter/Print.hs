module Interpreter.Print where

import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map.Strict as Map


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
      print' (Data con'tag args) = do
        printed'vals <- mapM print' args
        case sequence printed'vals of
          Left err -> return $ Left err
          Right vals -> return $ Right $ "(" ++ con'tag ++ " " ++ unwords vals ++ ")"
