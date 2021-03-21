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
import Compiler.TypeChecker.Type
import Compiler.Syntax.Type
import Compiler.Syntax.Expression


main :: IO ()
main = do
  putStrLn "Glamorous Frea REPL."
  putStrLn ""
  handle <- openFile "prelude.fr" ReadMode
  contents <- hGetContents handle
  case parse'expr contents of
    Left (Define binds) -> do
      let
        close'with (env, binds) (name, expr) =
          let
            Val.Env env'map = env
            new'env = Val.Env $ Map.insert name (expr, env) env'map
            closed = (expr, new'env)
          in
            (new'env, (name, closed) : binds)
        (_, closed) = foldl close'with (Val.Env Map.empty, []) binds
        env' = Val.Env $ Map.fromList closed
        -- env' = Val.Env $ Map.fromList $ map (second (, Val.Env Map.empty)) binds
      case infer'env binds empty'env of
        Left err -> do
          putStrLn $ "Type Error in Prelude: " ++ show err
        Right (Env mp) -> do
          let (Env t'map) = empty'env
          let t'env' = Env $ mp `Map.union` t'map
          repl env' t'env' [t'Bool, t'Int, t'Double, t'Char, t'Unit]
      return ()
    _ -> do
      putStrLn "Error: Prelude is badly defined."


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
        Left (Define binds) -> do
          let
            -- :: (Env, [(String, (Expression, Env))]) -> (String, Expression) -> (Env, [(String, (Expression, Env))])
            close'with (env, binds) (name, expr) =
              let
                Val.Env env'map = env
                new'env = Val.Env $ Map.insert name (expr, env) env'map
                closed = (expr, new'env)
              in
                (new'env, (name, closed) : binds)
            (_, closed) = foldl close'with (env, []) binds
            env' = Val.Env $ Map.fromList closed `Map.union` env'map
          case infer'env binds t'env of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err
            Right (Env mp) -> do
              let t'env' = Env $ mp `Map.union` t'map

              -- loop              
              repl env' t'env' type'ctx
        Left (Data name constrs) ->
          case check'constrs constrs (TyCon name : type'ctx) of
            Left err -> putStrLn err
            Right _ -> do
              let t'env' = add'constrs (TyCon name) constrs t'env
              let env' = add'constr'insts constrs env

              repl env' t'env' type'ctx
              
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


check'constrs :: [Constr] -> [Type] -> Either String ()
check'constrs [] _ = Right ()
check'constrs (Con name types : cons) type'ctx = do
  check types
    where
      check :: [Type] -> Either String ()
      check [] = Right ()
      check (t : ts) =
        case t of
          TyVar _ -> Right ()
          TyCon n
            | t `elem` type'ctx -> Right ()
            | otherwise -> Left $ "Type error: Unknown type constructor " ++ n ++ "." 
          TyTuple types -> do
            check types
          TyList t -> do
            check [t]
          TyArr from't to't -> do
            check [from't]
            check [to't]


add'constrs :: Type -> [Constr] -> TypeEnv -> TypeEnv
add'constrs _ [] t'env = t'env
add'constrs result't (Con name types : cons) (Env t'env)
  = add'constrs result't cons (Env $ Map.insert name scheme t'env)
    where
      type' = foldr TyArr result't types
      scheme = ForAll [] type'


add'constr'insts :: [Constr] -> Val.Env -> Val.Env
add'constr'insts [] env = env
add'constr'insts (Con name types : cons) (Val.Env env)
  = add'constr'insts cons (Val.Env $ Map.insert name (con'lam, Val.Env Map.empty) env)
    where
      par'inds = [1 .. length types]
      params = map (\ ind -> "p" ++ show ind ) par'inds
      vars = map Var params
      intro = Intro name vars
      con'lam = foldr Lam intro params
