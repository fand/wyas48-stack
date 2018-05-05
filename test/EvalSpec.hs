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

    it "string?" $ do
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

    it "if" $ do
      re "(if (> 2 3) \"foo\" \"bar\")" `shouldBe` String "bar"
      re "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")" `shouldBe` Number 9

    it "car, cdr" $ do
      re "(cdr '(a simple test))" `shouldBe` List [Atom "simple", Atom "test"]
      re "(car (cdr '(a simple test)))" `shouldBe` Atom "simple"
      re "(car '((this is) a test))" `shouldBe` List [Atom "this", Atom "is"]
      re "(cons '(this is) 'test)" `shouldBe` DottedList [List [Atom "this", Atom "is"]] (Atom "test")
      re "(cons '(this is) '())" `shouldBe` List [List [Atom "this", Atom "is"]]

    it "eqv?" $ do
      re "(eqv? 1 3)" `shouldBe` Bool False
      re "(eqv? 3 3)" `shouldBe` Bool True
      re "(eqv? 'atom 'atom)" `shouldBe` Bool True

    it "equal?" $ do
      re "(equal? 123 123)" `shouldBe` Bool True
      re "(equal? 123 \"123\")" `shouldBe` Bool True
      re "(equal? 123 \"foo\")" `shouldBe` Bool False

    it "throws" $ do
      le "(symbol? 1 2)" `shouldStartWith` "Expected 1 args;"
      le "(string? 1 2)" `shouldStartWith` "Expected 1 args;"
      le "(number? 1 2)" `shouldStartWith` "Expected 1 args;"
      le "(boolean? 1 2)" `shouldStartWith` "Expected 1 args;"
      le "(+ 2 \"two\")" `shouldStartWith` "Invalid type:"
      le "(+ 2)" `shouldStartWith` "Expected 2 args;"
      le "(what? 2)" `shouldStartWith` "Unrecognized primitive function args:"
      le "(if 1 2 3)" `shouldStartWith` "Invalid type:"
