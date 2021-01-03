module Main where


import System.IO
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))
import Control.Monad (forM_)

import Data.List (intercalate)

import Compiler.Parser.Parser (parse'expr)
import Compiler.Parser.Lexer (readToken)
import Compiler.Parser.Token (Token (..))
import Compiler.Parser.Utils
import Compiler.TypeChecker.Inference


noSourceFileError :: String
noSourceFileError =
  "I need to be given sourcefile to work with.\
  \\n\
  \\n\
  \Try something like: ggc main.gs"


readtoks :: P [Token]
readtoks = do
            t <- readToken
            case t of
              TokEOF -> return [t]
              _ -> do 
                rest <- readtoks
                return (t : rest)

tokenize::String-> [Token]
tokenize s = 
        evalP readtoks s 




main :: IO ()
main = do
  putStrLn "Glamorous Frea Compiler."
  putStrLn ""
  putStrLn ""
  repl


repl :: IO ()
repl = do
  putStr "frea λ> "
  hFlush stdout
  -- read
  line <- getLine

  -- eval
  case line of
    [] -> do
      putStrLn ""
      repl
    "::exit" ->
      return ()
    _ -> do
      let expression = parse'expr line -- module'
      let error'or'type = inferExpression empty'env expression
      -- print
      case error'or'type of
        Left err -> putStrLn $ "Type Error: " ++ show err
        Right type' -> putStrLn $ show type'

      -- loop
      repl
