{-# LANGUAGE TupleSections #-}
module Main where


import System.IO
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Bifunctor (second)
import Control.Monad.State.Lazy

import Data.List (intercalate, reverse)
import Data.List.Extra

import Compiler.Parser.Parser (parse'expr)
import Compiler.Parser.Lexer (readToken)

import Compiler.Parser.Token (Token (..))
import Compiler.Parser.Utils
import Compiler.TypeChecker.Inference
import Interpreter.Evaluate
import Interpreter.Value
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Type
import Compiler.Syntax.Type
import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration
import Compiler.TypeChecker.DeclarationCheck
import Compiler.TypeChecker.Inference.TypeOf
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.TypeEnv
import Compiler.KindChecker.KindEnv
import Compiler.KindChecker.KindError


main :: IO ()
main = do
  putStrLn "Glamorous Frea REPL."
  putStrLn ""
  load "prelude.fr" empty'env empty't'env empty'k'env empty'memory


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


repl :: Env -> TypeEnv -> KindEnv -> Memory -> IO ()
repl env t'env k'env mem = do
  -- read
  line <- readExpression

  -- evaluate
  case line of
    [] -> do
      putStrLn ""

      -- loop
      repl env t'env k'env mem
    ":exit" -> do
      putStrLn "Bye!"
      return ()
    ':' : 'l' : 'o' : 'a' : 'd' : ' ' : file -> do
      let trimmed = trim file
      load trimmed env t'env k'env mem

    ":q" -> do
      putStrLn "Bye!"
      return ()
    ":Q" -> do
      putStrLn "Bye!"
      return ()

    -- COMMAND :t(ype)
    ':' : 't' : line -> do
      case parse'expr line of
        Left _ -> do
          putStrLn "Incorrect Format! The :t command must be followed by an expression, not a declaration."

          -- loop
          repl env t'env k'env mem
        Right expression -> do
          let error'or'type = infer'expression t'env expression
          -- print
          case error'or'type of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env t'env k'env mem
            Right type' -> do
              putStrLn $ "frea λ > " ++ show expression ++ " :: " ++ show type'

              -- loop
              repl env t'env k'env mem

    -- COMMAND :k(ind)
    ':' : 'k' : line -> do
      putStrLn "Sorry, I can't parse type expressions yet.\n"
      -- NOTE: for now, just print all the known type constructors with their kinds
      putStrLn $ intercalate "\n" $ map (\ (name, kind) -> name ++ " :: " ++ show kind) $ Map.toList k'env
      repl env t'env k'env mem

    -- EXPRESSION to typecheck and evaluate
    _ -> do
      case parse'expr line of
        Left declarations -> do
          case process'declarations declarations env t'env k'env mem of
            Left err -> do
              putStrLn err
              repl env t'env k'env mem
            Right (env', t'env', k'env', mem') -> do
              case infer'env declarations t'env' of
                Left err -> do
                  putStrLn $ "Type Error in the declaration list: " ++ show err
                  return ()
                Right mp -> do
                  let t'env' = mp `Map.union` t'env
                  repl env' t'env' k'env' mem'

        Right expression -> do
          let error'or'type = infer'expression t'env expression
          case error'or'type of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env t'env k'env mem
            _ -> do
              let error'or'expr'n'state = runState (force expression env) mem
              -- print
              case error'or'expr'n'state of
                (Left err, mem') -> do
                  putStrLn $ "Evaluation Error: " ++ show err

                  -- loop
                  repl env t'env k'env mem

                (Right expr', mem') -> do
                  -- TODO: this is WRONG! present for whatever reason doesn't work well
                  -- so when you try to force the value for printing it doesn't update the memory or smth
                  -- so it breaks
                  putStrLn $ "         " ++ show expr'

                  -- loop
                  repl env t'env k'env mem' 


load :: String -> Env -> TypeEnv -> KindEnv -> Memory -> IO ()
load file'name env t'env k'env mem = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle
  case parse'expr contents of
    Left declarations -> do
      case process'declarations declarations env t'env k'env mem of
        Left err -> do
          putStrLn $ "Declaration Error inside " ++ file'name ++ ": " ++ err
          return ()
        Right (env', t'env, k'env', mem') -> do
          case infer'env declarations t'env of
            Left err -> do
              putStrLn $ "Type Error inside " ++ file'name ++ ": " ++ show err
              return ()
            Right mp -> do
              let t'env' = mp `Map.union` t'env
              repl env' t'env' k'env' mem'
    _ -> do
      putStrLn $ "Error: " ++ file'name ++ " must only contain declarations."