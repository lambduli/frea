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

import Compiler.TypeChecker.TypeOf
import Compiler.TypeChecker.AnalyzeEnv
import Compiler.TypeChecker.Analyze
import Compiler.TypeChecker.Kind.KindOf

import Interpreter.Evaluate
import Interpreter.Value
import qualified Interpreter.Print as IP


-- import Compiler.TypeChecker.Inference
-- import Compiler.TypeChecker.TypeError
-- import Compiler.TypeChecker.Type
-- import Compiler.TypeChecker.DeclarationCheck
-- import Compiler.TypeChecker.Inference.TypeOf
-- import Compiler.TypeChecker.Inference.Infer

-- import Compiler.KindChecker.KindEnv
-- import Compiler.KindChecker.KindError
-- import Compiler.KindChecker.Inference
-- import Compiler.KindChecker.InferState


main :: IO ()
main = do
  putStrLn "Glamorous Frea REPL."
  putStrLn ""
  load "prelude.frea" empty'env empty't'env empty'k'env Map.empty empty'memory


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


repl :: Env -> TypeEnv -> KindEnv -> AliEnv -> Memory -> IO ()
repl env t'env k'env ali'env mem = do
  -- read
  line <- readExpression

  -- evaluate
  case line of
    [] -> do
      putStrLn ""

      -- loop
      repl env t'env k'env ali'env mem
    ":exit" -> do
      putStrLn "Bye!"
      return ()
    ':' : 'l' : 'o' : 'a' : 'd' : ' ' : file -> do
      let trimmed = trim file
      load trimmed env t'env k'env ali'env mem

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
          repl env t'env k'env ali'env mem
        Right expression -> do
          let error'or'scheme = run'analyze (k'env, t'env, ali'env) (infer'expression expression) -- (infer'expression expression)
          -- print
          case error'or'scheme of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env t'env k'env ali'env mem
            Right scheme -> do
              -- let error'or'expr'n'state = runState (IP.print $ expression en) mem
              -- case error'or'expr'n'state of
                -- (Left err, mem') -> do
                --   putStrLn $ "Evaluation Error when Printing the value: " ++ show err

                --   -- loop
                --   repl env t'env k'env mem

                -- (Right str', mem') -> do
                  -- TODO: this is WRONG! present for whatever reason doesn't work well
                  -- so when you try to force the value for printing it doesn't update the memory or smth
                  -- so it breaks
                  putStrLn $ "         " ++ trim line ++ " :: " ++ show scheme

                  -- putStrLn $ "         " ++ str'

                  -- loop
                  repl env t'env k'env ali'env mem



              -- loop
              -- repl env t'env k'env mem

    -- COMMAND :k(ind)
    ':' : 'k' : line -> do
      let t = parse'type line
      let error'or'kind = kind'of (k'env, t'env, ali'env) t -- infer'kind k'env t -- runExcept $ evalStateT (runReaderT (infer'kind t) k'env) init'infer
      case error'or'kind of
        Left err -> do
          putStrLn $ "Kind Error: " ++ show err

          -- loop
          repl env t'env k'env ali'env mem

        Right kind' -> do
          putStrLn $ "         " ++ show t ++ " :: " ++ show kind'

          -- loop
          repl env t'env k'env ali'env mem

      -- putStrLn "Sorry, I can't parse type expressions yet.\n"
      -- NOTE: for now, just print all the known type constructors with their kinds
      -- putStrLn $ intercalate "\n" $ map (\ (name, kind) -> name ++ " :: " ++ show kind) $ Map.toList k'env
      -- repl env t'env k'env mem

    -- EXPRESSION to typecheck and evaluate
    _ -> do
      case parse'expr line of
        Left declarations -> do
          case run'analyze (k'env, t'env, ali'env) (analyze'module declarations (env, mem)) of
            Left err -> do
              putStrLn $ "Error " ++ show err
              return ()
            Right (k'e, t'e, a'e, e, m) ->
              repl e t'e k'e a'e m


          -- case process'declarations declarations env t'env k'env mem of
          --   Left err -> do
          --     putStrLn err
          --     repl env t'env k'env ali'env mem
          --   Right (env', t'env', k'env', mem') -> do
          --     case infer'env declarations t'env' of
          --       Left err -> do
          --         putStrLn $ "Type Error in the declaration list: " ++ show err
          --         return ()
          --       Right mp -> do
          --         let t'env' = mp `Map.union` t'env
          --         repl env' t'env' k'env' ali'env mem'

        Right expression -> do
          let error'or'scheme = run'analyze (k'env, t'env, ali'env) (infer'expression expression)
          case error'or'scheme of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl env t'env k'env ali'env mem
            _ -> do
              let error'or'expr'n'state = runState (IP.print $ force expression env) mem
              -- print
              case error'or'expr'n'state of
                (Left err, mem') -> do
                  putStrLn $ "Evaluation Error: " ++ show err

                  -- loop
                  repl env t'env k'env ali'env mem

                (Right str', mem') -> do
                  -- TODO: this is WRONG! present for whatever reason doesn't work well
                  -- so when you try to force the value for printing it doesn't update the memory or smth
                  -- so it breaks
                  putStrLn $ "         " ++ str'

                  -- loop
                  repl env t'env k'env ali'env mem' 


load :: String -> Env -> TypeEnv -> KindEnv -> AliEnv -> Memory -> IO ()
load file'name env t'env k'env ali'env mem = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle
  case parse'expr contents of
    Left declarations -> do
      case run'analyze (k'env, t'env, ali'env) (analyze'module declarations (env, mem)) of
        Left err -> do
          putStrLn $ "Error inside module " ++ file'name ++ ": " ++ show err
          return ()
        Right (k'e, t'e, a'e, e, m) ->
          repl e t'e k'e a'e m
      
      
      
      
      -- case process'declarations declarations env t'env k'env mem of
      --   Left err -> do
      --     putStrLn $ "Declaration Error inside " ++ file'name ++ ": " ++ err
      --     return ()
      --   Right (env', t'env, k'env', mem') -> do
      --     case infer'env declarations t'env of
      --       Left err -> do
      --         putStrLn $ "Type Error inside " ++ file'name ++ ": " ++ show err
      --         return ()
      --       Right mp -> do
      --         let t'env' = mp `Map.union` t'env
      --         repl env' t'env' k'env' mem'
    _ -> do
      putStrLn $ "Error: " ++ file'name ++ " must only contain declarations."