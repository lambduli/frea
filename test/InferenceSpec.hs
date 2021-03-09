module InferenceSpec where

import Test.Hspec

import Compiler.Parser.Parser (parse'expr)
import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Literal

import Compiler.TypeChecker.Inference (typeof)


spec :: Spec
spec = describe "Test the inference" $ do
  it "Infers the type of a single integer" $ do
     typeof (Lit (LitInt 23)) `shouldBe` Right (ForAll [] $ TyCon "Int")
  it "Infers the type of an empty list" $ do
    -- this test is dependent on the naming strategy for type variables
    typeof (List []) `shouldBe` Right (ForAll ["a"] $ TyList (TyVar "a"))
  it "Infers the type of a tuple" $ do
    typeof (Tuple [Lit (LitInt 23), Lit (LitBool True), Lit (LitChar 'a')])
      `shouldBe`
      Right (ForAll [] $ TyTuple [TyCon "Int", TyCon "Bool", TyCon "Char"])
  it "Infers the type of a let inside lambda" $ do
    typeof (parse'expr "\\ x -> let y = ((#+) (x, 1)) in y")
      `shouldBe` Right (ForAll [] $ TyArr (TyCon "Int") (TyCon "Int"))
  it "Infers the type of a let inside lambda [prefix] ((+) x 1)" $ do
    typeof (parse'expr "\\ x -> let y = ((+) x 1) in y")
      `shouldBe` Right (ForAll [] $ TyArr (TyCon "Int") (TyCon "Int"))
  it "Infers the type of a let inside lambda [infix] (x + 1)" $ do
    typeof (parse'expr "\\ x -> let y = (x + 1) in y")
      `shouldBe` Right (ForAll [] $ TyArr (TyCon "Int") (TyCon "Int"))
  it "Infers the type of an equality check (on Int) inside the lambda" $ do
    typeof (parse'expr "(\\ x -> if ((#=) (x, 23)) then #t else #f)")
      `shouldBe` Right (ForAll [] $ TyArr (TyCon "Int") (TyCon "Bool"))
  it "Infers the type of polymorphic equality check inside the lambda" $ do
    -- this test is dependent on the naming strategy for type variables
    typeof (parse'expr "(\\ x y -> ((#=) (x, y)))")
      `shouldBe`
      Right (
        ForAll ["a"] $
          (TyVar "a" `TyArr` (TyVar "a" `TyArr` TyCon "Bool")))
  it "Infers the type of " $ do
    -- this test is dependent on the naming strategy for type variables
    typeof (parse'expr "(\\ x -> (1, x))")
      `shouldBe`
      Right (
        ForAll ["a"] $
        TyVar "a" `TyArr` TyTuple [TyCon "Int", TyVar "a"])
  it "Infers the type of a list of applications" $ do
    typeof (parse'expr "let fn = (\\ i -> i) in [(fn 23), (fn ((#+) (23, 1))), (fn 42)]")
      `shouldBe` Right (ForAll [] $ TyList $ TyCon "Int")
  
  -- TODO: add more test with infix operators and functions 
  it "Infers the type of a let with infix function expression" $ do
    typeof (parse'expr "let plus = (\\ a b -> ((#+) (a, b))) in (23 `plus` 42)")
      `shouldBe` Right (ForAll [] $ TyCon "Int")