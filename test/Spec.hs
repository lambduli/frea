import Test.Hspec

import qualified ParserSpec
import qualified ParserDeclSpec
import qualified InferenceSpec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Parsing test" ParserSpec.spec
  describe "Parsing declarations" ParserDeclSpec.spec
  describe "Inference test" InferenceSpec.spec

