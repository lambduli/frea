module Main where


import System.IO
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))

import qualified Data.Map.Strict as Map
import Data.Bifunctor (second)
import Data.List (intercalate, reverse)
import Data.List.Extra

import Control.Monad (forM_)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Lazy

import Compiler.Parser.Parser (parse'expr, parse'type)
import Compiler.Parser.Lexer (readToken)
import Compiler.Parser.Token (Token (..))
import Compiler.Parser.Utils

import Compiler.Syntax.Type
import Compiler.Syntax.Expression
import Compiler.Syntax.Declaration

import Compiler.TypeAnalyzer.TypeOf
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.Kind.KindOf

import Interpreter.Evaluate
import Interpreter.Value
import qualified Interpreter.Print as IP


main :: IO ()
main = do
  putStrLn "Glamorous Frea REPL."
  putStrLn ""
  load "prelude.frea" empty'env empty'an'env empty'memory


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


repl :: Env -> AnalyzeEnv -> Memory -> IO ()
repl env e@AEnv{ kind'env = k'env, type'env = t'env, ali'env = ali'env } mem = do
  -- read
  line <- readExpression

  -- evaluate
  case line of
    [] -> do
      putStrLn ""

      -- loop
      repl env e mem
    ":exit" -> do
      putStrLn "Bye!"
      return ()
    ':' : 'l' : 'o' : 'a' : 'd' : ' ' : file -> do
      let trimmed = trim file
      load trimmed env e mem

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
          repl env e mem
        Right expression -> do
          let error'or'scheme = run'analyze e (infer'expression expression)
          -- print
          case error'or'scheme of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env e mem
            Right scheme -> do
              putStrLn $ "         " ++ trim line ++ " :: " ++ show scheme

              -- loop
              repl env e mem

    -- COMMAND :k(ind)
    ':' : 'k' : line -> do
      let t = parse'type line
      let error'or'kind = kind'of e t -- infer'kind k'env t -- runExcept $ evalStateT (runReaderT (infer'kind t) k'env) init'infer
      case error'or'kind of
        Left err -> do
          putStrLn $ "Kind Error: " ++ show err

          -- loop
          repl env e mem

        Right kind' -> do
          putStrLn $ "         " ++ show t ++ " :: " ++ show kind'

          -- loop
          repl env e mem

    -- EXPRESSION to typecheck and evaluate
    _ -> do
      case parse'expr line of
        Left declarations -> do
          case run'analyze e (analyze'module declarations (env, mem)) of
            Left err -> do
              putStrLn $ "Error " ++ show err
              return ()
            Right (an'env, e, m) ->
              repl e an'env m

        Right expression -> do
          let error'or'scheme = run'analyze e (infer'expression expression)
          case error'or'scheme of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env e mem
            _ -> do
              let error'or'expr'n'state = runState (IP.print $ force expression env) mem
              -- print
              case error'or'expr'n'state of
                (Left err, mem') -> do
                  putStrLn $ "Evaluation Error: " ++ show err

                  -- loop
                  repl env e mem

                (Right str', mem') -> do
                  putStrLn $ "         " ++ str'

                  -- loop
                  repl env e mem' 


load :: String -> Env -> AnalyzeEnv -> Memory -> IO ()
load file'name env a'env mem = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle
  case parse'expr contents of
    Left declarations -> do
      case run'analyze a'env (analyze'module declarations (env, mem)) of
        Left err -> do
          putStrLn $ "Error inside module " ++ file'name ++ ": " ++ show err
          return ()
        Right (an'env, e, m) ->
          repl e an'env m

    _ -> do
      putStrLn $ "Error: " ++ file'name ++ " must only contain declarations."