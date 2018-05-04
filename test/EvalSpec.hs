module EvalSpec (spec) where

import           Data.Complex
import           Data.Ratio
import           Eval
import           Read
import           Test.Hspec
import           Types

spec :: Spec
spec =
  describe "eval" $
    let re x = extractValue $ readExpr x >>= eval in

    it "evaluates functions" $ do
      re "(+ 1 2 3)" `shouldBe` Number 6
      re "(- 3 1)" `shouldBe` Number 2
      re "(- 10 1 2)" `shouldBe` Number 7
      re "(* 1 2 3 4 5)" `shouldBe` Number 120
      re "(/ 23 7)" `shouldBe` Number 3
      re "(/ 51 7 3)" `shouldBe` Number 2
      re "(mod 7 3)" `shouldBe` Number 1
      re "(quotient 7 3)" `shouldBe` Number 2
      re "(remainder 7 3)" `shouldBe` Number 1
      re "(symbol? 'a)" `shouldBe` Bool True
      re "(symbol? 1)" `shouldBe` Bool False
      re "(string? \"foo\")" `shouldBe` Bool True
      re "(string? 1)" `shouldBe` Bool False
      re "(number? 1)" `shouldBe` Bool True
      re "(number? 'a)" `shouldBe` Bool False
      re "(boolean? #t)" `shouldBe` Bool True
      re "(boolean? 'f)" `shouldBe` Bool False
