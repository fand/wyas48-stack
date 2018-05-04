module EvalSpec (spec) where

import           Control.Monad.Error
import           Data.Complex
import           Data.Either
import           Data.Ratio
import           Eval
import           Read
import           Test.Hspec
import           Types

spec :: Spec
spec =
  let
    re x = extractValue $ readExpr x >>= eval
    le x = show . head $ lefts [readExpr x >>= eval]
  in

  describe "eval" $ do
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

      re "(= 1 1)" `shouldBe` Bool True
      re "(= 1 2)" `shouldBe` Bool False
      re "(> 1 0)" `shouldBe` Bool True
      re "(> 1 2)" `shouldBe` Bool False
      re "(< 1 0)" `shouldBe` Bool False
      re "(< 1 2)" `shouldBe` Bool True
      re "(/= 1 1)" `shouldBe` Bool False
      re "(/= 1 2)" `shouldBe` Bool True
      re "(>= 1 2)" `shouldBe` Bool False
      re "(>= 1 1)" `shouldBe` Bool True
      re "(<= 1 0)" `shouldBe` Bool False
      re "(<= 1 1)" `shouldBe` Bool True
      re "(&& #t #t)" `shouldBe` Bool True
      re "(&& #t #f)" `shouldBe` Bool False
      re "(&& #f #f)" `shouldBe` Bool False
      re "(|| #t #t)" `shouldBe` Bool True
      re "(|| #t #f)" `shouldBe` Bool True
      re "(|| #f #f)" `shouldBe` Bool False
      re "(string=? \"a\" \"a\")" `shouldBe` Bool True
      re "(string=? \"a\" \"b\")" `shouldBe` Bool False
      re "(string<? \"a\" \"a\")" `shouldBe` Bool False
      re "(string<? \"a\" \"b\")" `shouldBe` Bool True
      re "(string>? \"a\" \"a\")" `shouldBe` Bool False
      re "(string>? \"c\" \"b\")" `shouldBe` Bool True
      re "(string<=? \"b\" \"a\")" `shouldBe` Bool False
      re "(string<=? \"b\" \"b\")" `shouldBe` Bool True
      re "(string>=? \"a\" \"b\")" `shouldBe` Bool False
      re "(string>=? \"b\" \"b\")" `shouldBe` Bool True

    it "throws" $ do
      le "(symbol? 1 2)" `shouldStartWith` "LispError:"
      le "(string? 1 2)" `shouldStartWith` "LispError:"
      le "(number? 1 2)" `shouldStartWith` "LispError:"
      le "(boolean? 1 2)" `shouldStartWith` "LispError:"
      le "(+ 2 \"two\")" `shouldStartWith` "Invalid type:"
      le "(+ 2)" `shouldStartWith` "Expected 2 args;"
      le "(what? 2)" `shouldStartWith` "Unrecognized primitive function args:"
