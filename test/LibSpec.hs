module LibSpec (spec) where

import           Lib
import           Test.Hspec

spec :: Spec
spec =
  describe "readExpr" $ do
    it "parseString" $ do
      readExpr "\"foo\"" `shouldBe` "Found value: \"foo\""
      readExpr "\"foo\\\"bar\\\"baz\"" `shouldBe` "Found value: \"foo\"bar\"baz\""

    it "parseBool" $ do
      readExpr "#t" `shouldBe` "Found value: #t"
      readExpr "#f" `shouldBe` "Found value: #f"

    it "parseAtom" $
      readExpr "foo" `shouldBe` "Found value: foo"

    it "parseHex" $
      readExpr "#xFF" `shouldBe` "Found value: 255"

    it "parseDigits" $ do
      readExpr "#o101" `shouldBe` "Found value: 65"
      readExpr "#d123" `shouldBe` "Found value: 123"
      readExpr "#b101" `shouldBe` "Found value: 5"

    it "parseDecimal" $
      readExpr "123" `shouldBe` "Found value: 123"

    it "parseCharacter" $ do
      readExpr "#\\newline" `shouldBe` "Found value: #\\newline"
      readExpr "#\\space" `shouldBe` "Found value: #\\space"
      readExpr "#\\a" `shouldBe` "Found value: #\\a"

    it "parseFloat" $ do
      readExpr "1.0" `shouldBe` "Found value: 1.0"
      readExpr "0.1" `shouldBe` "Found value: 0.1"
      readExpr ".0" `shouldStartWith` "No match:"

    it "parseRatio" $ do
      readExpr "1/2" `shouldBe` "Found value: 1 % 2"
      readExpr "2/4" `shouldBe` "Found value: 1 % 2"

    it "parseComplex" $ do
      readExpr "1+2i" `shouldBe` "Found value: 1.0 :+ 2.0"
      readExpr "1.23+2.46i" `shouldBe` "Found value: 1.23 :+ 2.46"


    it "parseList" $ do
      readExpr "(a test)" `shouldBe` "Found value: (a test)"
      readExpr "(a (nested) test)" `shouldBe` "Found value: (a (nested) test)"
      readExpr "(a test" `shouldStartWith` "No match:"

    it "parseDottedList" $ do
      readExpr "(a b . c)" `shouldBe` "Found value: (a b . c)"
      readExpr "(a (b . c) d)" `shouldBe` "Found value: (a (b . c) d)"

    it "parseQuoted" $ do
      readExpr "'(a b)" `shouldBe` "Found value: (quote (a b))"
      readExpr "(a 'b c)" `shouldBe` "Found value: (a (quote b) c)"
