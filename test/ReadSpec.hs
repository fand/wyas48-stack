module ReadSpec (spec) where

import           Data.Complex
import           Data.Ratio
import           LispVal
import           Read
import           Test.Hspec

spec :: Spec
spec =
  describe "readExpr" $ do
    it "parseString" $ do
      readExpr "\"foo\"" `shouldBe` String "foo"
      readExpr "\"foo\\\"bar\\\"baz\"" `shouldBe` String "foo\"bar\"baz"

    it "parseBool" $ do
      readExpr "#t" `shouldBe` Bool True
      readExpr "#f" `shouldBe` Bool False

    it "parseAtom" $
      readExpr "foo" `shouldBe` Atom "foo"

    it "parseHex" $
      readExpr "#xFF" `shouldBe` Number 255

    it "parseDigits" $ do
      readExpr "#o101" `shouldBe` Number 65
      readExpr "#d123" `shouldBe` Number 123
      readExpr "#b101" `shouldBe` Number 5

    it "parseDecimal" $
      readExpr "123" `shouldBe` Number 123

    it "parseCharacter" $ do
      readExpr "#\\newline" `shouldBe` Character '\n'
      readExpr "#\\space" `shouldBe` Character ' '
      readExpr "#\\a" `shouldBe` Character 'a'

    it "parseFloat" $ do
      readExpr "1.0" `shouldBe` Float 1.0
      readExpr "0.1" `shouldBe` Float 0.1
      -- readExpr ".0" `shouldStartWith` String "No match:"

    it "parseRatio" $ do
      readExpr "1/2" `shouldBe` Ratio (1 % 2)
      readExpr "2/4" `shouldBe` Ratio (1 % 2)

    it "parseComplex" $ do
      readExpr "1+2i" `shouldBe` Complex (1.0 :+ 2.0)
      readExpr "1.23+2.46i" `shouldBe` Complex (1.23 :+ 2.46)


    it "parseList" $ do
      readExpr "(a test)" `shouldBe` List [Atom "a", Atom "test"]
      readExpr "(a (nested) test)" `shouldBe` List[Atom "a", List[Atom "nested"], Atom "test"]
      -- readExpr "(a test" `shouldStartWith` "No match:"

    it "parseDottedList" $ do
      readExpr "(a b . c)" `shouldBe` DottedList [Atom "a", Atom "b"] (Atom "c")
      readExpr "(a (b . c) d)" `shouldBe` List [Atom "a", DottedList [Atom "b"] (Atom "c"), Atom "d"]

    it "parseQuoted" $ do
      readExpr "'(a b)" `shouldBe` List [Atom "quote", List [Atom "a", Atom "b"]]
      readExpr "(a 'b c)" `shouldBe` List [Atom "a", List [Atom "quote", Atom "b"], Atom "c"]
