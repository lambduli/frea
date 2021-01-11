import Test.Hspec

import qualified ParserSpec
import qualified InferenceSpec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Parsing test" ParserSpec.spec
  describe "Inference test" InferenceSpec.spec

