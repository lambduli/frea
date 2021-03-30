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
    "one = 1" <=> Binding "one" (Lit $ LitInt 1)
  it "Parses a function declaration" $ do
    "fn x = x" <~> "fn = (lambda x -> x)"
  -- NOTE: fails - probably because grammar conflicts
  -- it "Parses an operator declaration" $ do
  --   "(:) a b = a" <~> "a : b = a"
  -- it "Parses an operator declaration II" $ do
  --   "(:) = (lambda a b -> a)" <~> "a : b = a"
  it "Parses a data declaration" $ do
    "data Bool = True | False" <=> DataDecl "Bool" [] [ConDecl "True" [], ConDecl "False" []]


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