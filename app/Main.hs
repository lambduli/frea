module Main where


import System.IO
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))
import Control.Monad (forM_)

import Data.List (intercalate, reverse)

import Compiler.Parser.Parser (parse'expr)
import Compiler.Parser.Lexer (readToken)
import Compiler.Parser.Token (Token (..))
import Compiler.Parser.Utils
import Compiler.TypeChecker.Inference
import Interpreter.Evaluate


main :: IO ()
main = do
  putStrLn "Glamorous Frea REPL."
  putStrLn ""
  repl


readExpression :: IO (String)
readExpression = do
  putStr "frea λ> "
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
        putStr "        "
        hFlush stdout
        line <- getLine
        case line of
          "" -> return line
          ':' : 'e' : 'x' : 'i' : 't' : _ -> return line
          ':' : 'q' : _ -> return line
          _ -> do
            next'line <- read'expr'
            return $ line ++ ['\n'] ++ next'line


repl :: IO ()
repl = do
  -- read
  line <- readExpression

  -- evaluate
  case line of
    [] -> do
      putStrLn ""
      repl
    ":exit" -> do
      putStrLn "Bye!"
      return ()
    ":q" -> do
      putStrLn "Bye!"
      return ()
    ':' : 't' : line -> do
      let expression = parse'expr line
      let error'or'type = infer'expression empty'env expression
      -- print
      case error'or'type of
        Left err -> do
          putStrLn $ "Type Error: " ++ show err
          repl
        Right type' -> do
          putStrLn $ "frea λ> :: " ++ show type'
          repl
        
    _ -> do
      let expression = parse'expr line
      let error'or'type = infer'expression empty'env expression
      case error'or'type of
        Left err -> do
          putStrLn $ "Type Error: " ++ show err
          repl
        _ -> do
          let error'or'expr = evaluate expression
          -- print
          case error'or'expr of
            Left err -> putStrLn $ "Evaluation Error: " ++ show err
            Right expr' -> putStrLn $ show expr'

          -- loop
          repl
