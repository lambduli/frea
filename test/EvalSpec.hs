module EvalSpec where

import Test.Hspec
import System.Exit
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'expr)
import Interpreter.Value
import Interpreter.Evaluate
import Compiler.Syntax.Literal


spec :: Spec
spec = describe "Test evaluation of expressions" $ do
  it "Evaluates simple constant" $ do
    "1" `evals'to` Lit (LitInt 1)
  it "Evaluates simple lambda application" $ do
    "((lambda x -> x) 23)" `evals'to` Lit (LitInt 23)



evals'to :: String -> Value -> IO ()
expr `evals'to` val = do
  case parse'expr expr of
    Left _ -> exitFailure
    Right expr -> do
      let error'or'expr'n'state = runState (force expr Map.empty) Map.empty
      case error'or'expr'n'state of
        (Left err, mem') -> exitFailure 
        (Right expr', mem') -> show expr' `shouldBe` show val