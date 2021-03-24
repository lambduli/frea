module InferenceSpec where

import Test.Hspec
import System.Exit

import Compiler.Parser.Parser (parse'expr)
import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.TypeChecker.TypeError
import Compiler.Syntax.Literal

import Compiler.TypeChecker.TypeOf (typeof)
import Compiler.TypeChecker.Type


spec :: Spec
spec = describe "Test the inference" $ do

  it "Infers the type of a single integer" $
    typeof (Lit (LitInt 23)) `shouldBe` Right (ForAll [] t'Int)

  it "Infers the type of an empty list" $
    typeof (List []) `shouldBe` Right (ForAll ["a"] $ TyList (TyVar "a"))

  it "Infers the type of a tuple" $
    typeof (Tuple [Lit (LitInt 23), Lit (LitBool True), Lit (LitChar 'a')])
    `shouldBe`
    Right (ForAll [] $ TyTuple [t'Int, t'Bool, t'Char])

  it "Infers the type of a let inside lambda" $
    "\\ x -> let y = ((#+) (x, 1)) in y" <::> ForAll [] (TyArr t'Int t'Int)

  it "Infers the type of a let inside lambda [prefix] ((+) x 1)" $
    "\\ x -> let (+) = (\\ a b -> ((#+) (a, b))) y = ((+) x 1) in y" <::> ForAll [] (TyArr t'Int t'Int)

  it "Infers the type of a let inside lambda [infix] (x + 1)" $
    "\\ x -> let (+) = (\\ a b -> ((#+) (a, b))) y = (x + 1) in y" <::> ForAll [] (TyArr t'Int t'Int)

  it "Infers the type of an equality check (on Int) inside the lambda" $
    "(\\ x -> if ((#=) (x, 23)) then #t else #f)" <::> ForAll [] (TyArr t'Int t'Bool)

  it "Infers the type of polymorphic equality check inside the lambda" $
    "(\\ x y -> ((#=) (x, y)))" <::> ForAll ["a"] (TyVar "a" `TyArr` (TyVar "a" `TyArr` t'Bool))

  it "Infers the type of a polymorphic tuple" $
    "(\\ x -> (1, x))" <::> ForAll ["a"] (TyVar "a" `TyArr` TyTuple [t'Int, TyVar "a"])

  it "Infers the type of a list of applications" $
    "let fn = (\\ i -> i) in [(fn 23), (fn ((#+) (23, 1))), (fn 42)]" <::> ForAll [] (TyList t'Int)

  -- TODO: add more test with infix operators and functions 
  it "Infers the type of a let with infix function expression" $
    "let plus = (\\ a b -> ((#+) (a, b))) in (23 `plus` 42)" <::> ForAll [] t'Int


infix 4 <::>

(<::>) :: String -> Scheme -> IO ()
(<::>) expr scheme =
  case parse'expr expr of
    Left cmd -> exitFailure
    Right ast ->
      typeof ast
        `shouldBe` Right scheme
