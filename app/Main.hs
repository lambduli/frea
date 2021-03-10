{-# LANGUAGE TupleSections #-}
module Main where


import System.IO
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Bifunctor (second)

import Data.List (intercalate, reverse)

import Compiler.Parser.Parser (parse'expr)
import Compiler.Parser.Lexer (readToken)

import Compiler.Parser.Token (Token (..))
import Compiler.Parser.Utils
import Compiler.TypeChecker.Inference
import Interpreter.Evaluate
import qualified Interpreter.Value as Val
import Interpreter.Command
import Compiler.TypeChecker.TypeError


main :: IO ()
main = do
  putStrLn "Glamorous Frea REPL."
  putStrLn ""
  handle <- openFile "prelude.fr" ReadMode
  contents <- hGetContents handle
  case parse'expr contents of
    Left (Assume binds) -> do
      let
        env' = Val.Env $ map (second (, Val.Env [])) binds
      case infer'env binds empty'env of
        Left err -> do
          putStrLn $ "Type Error in Prelude: " ++ show err
        Right (Env mp) -> do
          let (Env t'map) = empty'env
          let t'env' = Env $ mp `Map.union` t'map
          repl env' t'env'
      return ()
    _ -> do
      putStrLn "Error: Prelude is badly defined."

  -- repl (Val.Env []) (empty'env)
  -- return ()


readExpression :: IO String
readExpression = do
  putStr "frea λ > "
  hFlush stdout
  line <- getLine
  case line of
    "" -> return line
    ':' : 'e' : 'x' : 'i' : 't' : _ -> return line
    ':' : 'q' : _ -> return line
    _ -> do
      next'line <- read'expr'
      return $ line ++ ['\n'] ++ next'line
    where
      read'expr' = do
        putStr "         "
        hFlush stdout
        line <- getLine
        case line of
          "" -> return line
          ':' : 'e' : 'x' : 'i' : 't' : _ -> return line
          ':' : 'q' : _ -> return line
          _ -> do
            next'line <- read'expr'
            return $ line ++ ['\n'] ++ next'line


repl :: Val.Env -> TypeEnv -> IO ()
repl env@(Val.Env bs) t'env@(Env t'map) = do
  -- read
  line <- readExpression

  -- evaluate
  case line of
    [] -> do
      putStrLn ""
      repl env t'env
    ":exit" -> do
      putStrLn "Bye!"
      return ()
    ":q" -> do
      putStrLn "Bye!"
      return ()
    ':' : 't' : line -> do
      case parse'expr line of
        Left (Assume binds) -> do
          putStrLn "Incorrect Format! An expression must follow the :t command, not a declaration."
          repl env t'env
        Right expression -> do
          let error'or'type = infer'expression t'env expression
          -- print
          case error'or'type of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err
              repl env t'env
            Right type' -> do
              putStrLn $ "frea λ > :: " ++ show type'
              repl env t'env
          
    _ -> do
      case parse'expr line of
        Left (Assume binds) -> do
          let
            env' = Val.Env $ bs ++ map (second (, env)) binds
          case infer'env binds t'env of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err
            Right (Env mp) -> do
              let t'env' = Env $ mp `Map.union` t'map
              repl env' t'env'
              
        Right expression -> do
          let error'or'type = infer'expression t'env expression
          case error'or'type of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err
              repl env t'env
            _ -> do
              let error'or'expr = evaluate expression env
              -- print
              case error'or'expr of
                Left err -> putStrLn $ "Evaluation Error: " ++ show err
                Right expr' -> putStrLn $ "         " ++ show expr'

              -- loop
              repl env t'env
