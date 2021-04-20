module ParserDeclSpec where

import Test.Hspec
import System.Exit

import Compiler.Parser.Parser (parse'expr)
import Compiler.Syntax.Declaration
import Compiler.Syntax.Expression
import Compiler.Syntax.Literal


spec :: Spec
spec = describe "Test parsing of the declarations" $ do
  it "Parses simple constant declaration" $ do
    "module Main where { one = 1 }" <=> Binding "one" (Lit $ LitInt 1)
  
  it "Parses a function declaration" $ do
    "module Main where { fn x = x }" <~> "module Main where { fn = lambda x -> x }"
  -- NOTE: fails - probably because grammar conflicts
  -- it "Parses an operator declaration" $ do
  --   "module Main where { (:) a b = a }" <~> "module Main where { a : b = a }"
  -- it "Parses an operator declaration II" $ do
  --   "module Main where { (:) = (lambda a b -> a)}" <~> "module Main where { a : b = a }"
  
  it "Parses a data declaration" $ do
    "module Main where { data Bool = True | False }" <=> DataDecl "Bool" [] [ConDecl "True" [], ConDecl "False" []]


infix 4 <=>

(<=>) :: String -> Declaration -> IO ()
(<=>) expr reference = do
  case parse'expr expr of
    Left [decl] -> show decl `shouldBe` show reference
    Right ast' -> exitFailure


infix 4 <~>

(<~>) :: String -> String -> IO ()
(<~>) expr'l expr'r = do
  case (parse'expr expr'l, parse'expr expr'r) of
    (l, r) -> show l `shouldBe` show r


oK :: String -> IO ()
oK expr = do
  case parse'expr expr of
    Left decls -> return ()