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
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Type
import Compiler.Syntax.Type
import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration
import Compiler.TypeChecker.DeclarationCheck
import Compiler.TypeChecker.Inference.Utils
import Compiler.TypeChecker.Inference.TypeOf
import Compiler.TypeChecker.Inference.Infer



main :: IO ()
main = do
  putStrLn "Glamorous Frea REPL."
  putStrLn ""
  handle <- openFile "prelude.fr" ReadMode
  contents <- hGetContents handle
  case parse'expr contents of
    Left declarations -> do
      case process'declarations declarations (Val.Env Map.empty) empty'env [t'Bool, t'Int, t'Double, t'Char, t'Unit] of
        Left err -> do
          putStrLn $ "Error in Prelude: " ++ err
          return ()
        Right (env', t'env, type'ctx) -> do
          case infer'env declarations t'env of
            Left err -> do
              putStrLn $ "Type Error in Prelude: " ++ show err
              return ()
            Right (Env mp) -> do
              let (Env t'map) = t'env
              let t'env' = Env $ mp `Map.union` t'map
              repl env' t'env' type'ctx

    _ -> do
      putStrLn "Error: Prelude must only contain declarations."


readExpression :: IO String
readExpression = do
  putStr "frea λ > "
  hFlush stdout
  line <- getLine
  case line of
    "" -> return line
    ':' : 'e' : 'x' : 'i' : 't' : _ -> return line
    ':' : 'q' : _ -> return line
    ':' : 'Q' : _ -> return line
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
          ':' : 'Q' : _ -> return line
          _ -> do
            next'line <- read'expr'
            return $ line ++ ['\n'] ++ next'line


repl :: Val.Env -> TypeEnv -> [Type] -> IO ()
repl env@(Val.Env env'map) t'env@(Env t'map) type'ctx = do
  -- read
  line <- readExpression

  -- evaluate
  case line of
    [] -> do
      putStrLn ""

      -- loop
      repl env t'env type'ctx
    ":exit" -> do
      putStrLn "Bye!"
      return ()
    ":q" -> do
      putStrLn "Bye!"
      return ()
    ":Q" -> do
      putStrLn "Bye!"
      return ()
    ':' : 't' : line -> do
      case parse'expr line of
        Left _ -> do
          putStrLn "Incorrect Format! An expression must follow the :t command, not a declaration."

          -- loop
          repl env t'env type'ctx
        Right expression -> do
          let error'or'type = infer'expression t'env expression
          -- print
          case error'or'type of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env t'env type'ctx
            Right type' -> do
              putStrLn $ "frea λ > " ++ show expression ++ " :: " ++ show type'

              -- loop
              repl env t'env type'ctx
          
    _ -> do
      case parse'expr line of
        Left declarations -> do
          case process'declarations declarations env t'env type'ctx of
            Left err -> do
              putStrLn err
              repl env t'env type'ctx
            Right (env', t'env', type'ctx') -> do
              case infer'env declarations t'env' of
                Left err -> do
                  putStrLn $ "Type Error in Prelude: " ++ show err
                  return ()
                Right (Env mp) -> do
                  let (Env t'map) = t'env'
                  let t'env' = Env $ mp `Map.union` t'map
                  repl env' t'env' type'ctx'

        Right expression -> do
          let error'or'type = infer'expression t'env expression
          case error'or'type of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env t'env type'ctx
            _ -> do
              let error'or'expr = evaluate expression env
              -- print
              case error'or'expr of
                Left err -> putStrLn $ "Evaluation Error: " ++ show err
                Right expr' -> putStrLn $ "         " ++ show expr'

              -- loop
              repl env t'env type'ctx
