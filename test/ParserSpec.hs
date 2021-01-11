module ParserSpec where

import Test.Hspec

import Compiler.Parser.Parser (parse'expr)
import Compiler.Syntax.Expression
import Compiler.Syntax.Literal


spec :: Spec
spec = describe "Test the parser" $ do
  it "Parses a single integer" $ do
    parse'expr "23" `shouldBe` Lit (LitInt 23)
  it "Parses a single double" $ do
    parse'expr "24.42" `shouldBe` Lit (LitDouble 24.42)
  it "Parses a single char" $ do
    parse'expr "'c'" `shouldBe` Lit (LitChar 'c')
  it "Parses a single string" $ do
    parse'expr "\"hello\"" `shouldBe` Lit (LitString "hello")
  it "Parses a single boolean (True)" $ do
    parse'expr "#t" `shouldBe` Lit (LitBool True)
  it "Parses a single boolean (False)" $ do
    parse'expr "#f" `shouldBe` Lit (LitBool False)
  it "Parses a single unit" $ do
    parse'expr "()" `shouldBe` Lit LitUnit
  it "Parses a single empty list" $ do
    parse'expr "[]" `shouldBe` List []
  it "Parses a single short list" $ do
    parse'expr "[1, 2]" `shouldBe` List [Lit (LitInt 1), Lit (LitInt 2)]
  it "Parses a single tuple (Pair) of values" $ do
    parse'expr "(23, #t)" `shouldBe` Tuple [Lit (LitInt 23), Lit (LitBool True)]
  
  it "Parses a lambda function" $ do
    parse'expr "\\ a b -> a" `shouldBe`
      Lam "a" (Lam "b" (Var "a"))
  it "Parses an if expression" $ do
    parse'expr "if #t then 23 else 42" `shouldBe`
      If (Lit (LitBool True)) (Lit (LitInt 23)) (Lit (LitInt 42))
  it "Parses a let expression" $ do
    parse'expr "let a = 23 in a" `shouldBe`
      Let "a" (Lit (LitInt 23)) (Var "a")
  it "Parses a fix expression" $ do
    parse'expr "fix (\\ fn n -> (fn n))" `shouldBe`
      Fix (Lam "fn" (Lam "n" (App (Var "fn") (Var "n"))))
  it "Parses a let rec expression" $ do
    parse'expr "let rec fn n = (fn n) in (fn 2)" `shouldBe`
      Let
        "fn"
        (Fix (Lam "fn" (Lam "n" (App (Var "fn") (Var "n")))))
        (App (Var "fn") (Lit (LitInt 2)))
  it "Parses a simple arithmetic expression equivalent to 23 + 42" $ do
    parse'expr "(#+ (23, 42))" `shouldBe`
      App (Op "#+") (Tuple [Lit (LitInt 23), Lit (LitInt 42)])

