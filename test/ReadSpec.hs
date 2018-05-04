module ReadSpec (spec) where

import           Data.Complex
import           Data.Ratio
import           Read
import           Test.Hspec
import           Types

spec :: Spec
spec =
  let r = extractValue . readExpr in
  describe "readExpr" $ do
    it "parseString" $ do
      r "\"foo\"" `shouldBe` String "foo"
      r "\"foo\\\"bar\\\"baz\"" `shouldBe` String "foo\"bar\"baz"

    it "parseBool" $ do
      r "#t" `shouldBe` Bool True
      r "#f" `shouldBe` Bool False

    it "parseAtom" $
      r "foo" `shouldBe` Atom "foo"

    it "parseHex" $
      r "#xFF" `shouldBe` Number 255

    it "parseDigits" $ do
      r "#o101" `shouldBe` Number 65
      r "#d123" `shouldBe` Number 123
      r "#b101" `shouldBe` Number 5

    it "parseDecimal" $
      r "123" `shouldBe` Number 123

    it "parseCharacter" $ do
      r "#\\newline" `shouldBe` Character '\n'
      r "#\\space" `shouldBe` Character ' '
      r "#\\a" `shouldBe` Character 'a'

    it "parseFloat" $ do
      r "1.0" `shouldBe` Float 1.0
      r "0.1" `shouldBe` Float 0.1
      -- readExpr ".0" `shouldStartWith` String "No match:"

    it "parseRatio" $ do
      r "1/2" `shouldBe` Ratio (1 % 2)
      r "2/4" `shouldBe` Ratio (1 % 2)

    it "parseComplex" $ do
      r "1+2i" `shouldBe` Complex (1.0 :+ 2.0)
      r "1.23+2.46i" `shouldBe` Complex (1.23 :+ 2.46)

    it "parseList" $ do
      r "(a test)" `shouldBe` List [Atom "a", Atom "test"]
      r "(a (nested) test)" `shouldBe` List[Atom "a", List[Atom "nested"], Atom "test"]
      -- readExpr "(a test" `shouldStartWith` "No match:"

    it "parseDottedList" $ do
      r "(a b . c)" `shouldBe` DottedList [Atom "a", Atom "b"] (Atom "c")
      r "(a (b . c) d)" `shouldBe` List [Atom "a", DottedList [Atom "b"] (Atom "c"), Atom "d"]

    it "parseQuoted" $ do
      r "'(a b)" `shouldBe` List [Atom "quote", List [Atom "a", Atom "b"]]
      r "(a 'b c)" `shouldBe` List [Atom "a", List [Atom "quote", Atom "b"], Atom "c"]
