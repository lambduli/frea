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
import Data.List.Extra

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
import Compiler.TypeChecker.Inference.TypeOf
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.TypeEnv



main :: IO ()
main = do
  putStrLn "Glamorous Frea REPL."
  putStrLn ""
  load "prelude.fr" (Val.Env Map.empty) empty'env Map.empty [t'Bool, t'Int, t'Double, t'Char, t'Unit]


readExpression :: IO String
readExpression = do
  putStr "frea λ > "
  hFlush stdout
  line <- getLine
  case line of
    "" -> return line
    ':' : 'e' : 'x' : 'i' : 't' : _ -> return line
    ':' : 'l' : 'o' : 'a' : 'd' : ' ' : _ -> return line
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
          ':' : 'l' : 'o' : 'a' : 'd' : ' ' : _ -> return line
          ':' : 'q' : _ -> return line
          ':' : 'Q' : _ -> return line
          _ -> do
            next'line <- read'expr'
            return $ line ++ ['\n'] ++ next'line


repl :: Val.Env -> TypeEnv -> Val.Memory -> [Type] -> IO ()
repl env@(Val.Env env'map) t'env@(Env t'map) mem type'ctx = do
  -- read
  line <- readExpression

  -- evaluate
  case line of
    [] -> do
      putStrLn ""

      -- loop
      repl env t'env mem type'ctx
    ":exit" -> do
      putStrLn "Bye!"
      return ()
    ':' : 'l' : 'o' : 'a' : 'd' : ' ' : file -> do
      let trimmed = trim file
      load trimmed env t'env mem type'ctx

    ":q" -> do
      putStrLn "Bye!"
      return ()
    ":Q" -> do
      putStrLn "Bye!"
      return ()

    -- COMMAND :T(ype)
    ':' : 't' : line -> do
      case parse'expr line of
        Left _ -> do
          putStrLn "Incorrect Format! The :t command must be followed by an expression, not a declaration."

          -- loop
          repl env t'env mem type'ctx
        Right expression -> do
          let error'or'type = infer'expression t'env expression
          -- print
          case error'or'type of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env t'env mem type'ctx
            Right type' -> do
              putStrLn $ "frea λ > " ++ show expression ++ " :: " ++ show type'

              -- loop
              repl env t'env mem type'ctx

    -- EXPRESSION to typecheck and evaluate
    _ -> do
      case parse'expr line of
        Left declarations -> do
          case process'declarations declarations env t'env mem type'ctx of
            Left err -> do
              putStrLn err
              repl env t'env mem type'ctx
            Right (env', t'env', type'ctx', mem') -> do
              case infer'env declarations t'env' of
                Left err -> do
                  putStrLn $ "Type Error in Prelude: " ++ show err
                  return ()
                Right (Env mp) -> do
                  let (Env t'map) = t'env'
                  let t'env' = Env $ mp `Map.union` t'map
                  repl env' t'env' mem' type'ctx'

        Right expression -> do
          let error'or'type = infer'expression t'env expression
          case error'or'type of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env t'env mem type'ctx
            _ -> do
              let error'or'expr = evaluate expression env mem
              -- print
              case error'or'expr of
                Left err -> putStrLn $ "Evaluation Error: " ++ show err
                Right expr' -> putStrLn $ "         " ++ Val.present mem expr'

              -- loop
              repl env t'env mem type'ctx


load :: String -> Val.Env -> TypeEnv -> Val.Memory -> [Type] -> IO ()
load file'name env@(Val.Env env'map) t'env@(Env t'map) mem type'ctx = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle
  case parse'expr contents of
    Left declarations -> do
      case process'declarations declarations env t'env mem type'ctx of
        Left err -> do
          putStrLn $ "Declaration Error inside " ++ file'name ++ ": " ++ err
          return ()
        Right (env', t'env, type'ctx, mem') -> do
          case infer'env declarations t'env of
            Left err -> do
              putStrLn $ "Type Error inside " ++ file'name ++ ": " ++ show err
              return ()
            Right (Env mp) -> do
              let (Env t'map) = t'env
              let t'env' = Env $ mp `Map.union` t'map
              repl env' t'env' mem' type'ctx
    _ -> do
      putStrLn $ "Error: " ++ file'name ++ " must only contain declarations."