module LibSpec (spec) where

import           Lib
import           Test.Hspec

spec :: Spec
spec =
  describe "readExpr" $ do
    it "parseString" $ do
      readExpr "\"foo\"" `shouldBe` "Found value: String \"foo\""
      readExpr "\"foo\\\"bar\\\"baz\"" `shouldBe` "Found value: String \"foo\\\"bar\\\"baz\""

    it "parseBool" $ do
      readExpr "#t" `shouldBe` "Found value: Bool True"
      readExpr "#f" `shouldBe` "Found value: Bool False"

    it "parseAtom" $
      readExpr "foo" `shouldBe` "Found value: Atom \"foo\""

    it "parseHex" $
      readExpr "#xFF" `shouldBe` "Found value: Number 255"

    it "parseDigits" $ do
      readExpr "#o101" `shouldBe` "Found value: Number 65"
      readExpr "#d123" `shouldBe` "Found value: Number 123"
      readExpr "#b101" `shouldBe` "Found value: Number 5"

    it "parseDecimal" $
      readExpr "123" `shouldBe` "Found value: Number 123"

    it "parseCharacter" $ do
      readExpr "#\\newline" `shouldBe` "Found value: Character '\\n'"
      readExpr "#\\space" `shouldBe` "Found value: Character ' '"
      readExpr "#\\a" `shouldBe` "Found value: Character 'a'"

    it "parseFloat" $ do
      readExpr "1.0" `shouldBe` "Found value: Float 1.0"
      readExpr "0.1" `shouldBe` "Found value: Float 0.1"
      readExpr ".0" `shouldStartWith` "No match:"

    it "parseRatio" $ do
      readExpr "1/2" `shouldBe` "Found value: Ratio (1 % 2)"
      readExpr "2/4" `shouldBe` "Found value: Ratio (1 % 2)"

    it "parseComplex" $ do
      readExpr "1+2i" `shouldBe` "Found value: Complex (1.0 :+ 2.0)"
      readExpr "1.23+2.46i" `shouldBe` "Found value: Complex (1.23 :+ 2.46)"


    it "parseList" $ do
      readExpr "(a test)" `shouldBe` "Found value: List [Atom \"a\",Atom \"test\"]"
      readExpr "(a (nested) test)" `shouldBe` "Found value: List [Atom \"a\",List [Atom \"nested\"],Atom \"test\"]"
      readExpr "(a test" `shouldStartWith` "No match:"

    it "parseDottedList" $ do
      readExpr "(a b . c)" `shouldBe` "Found value: DottedList [Atom \"a\",Atom \"b\"] (Atom \"c\")"
      readExpr "(a (b . c) d)" `shouldBe` "Found value: List [Atom \"a\",DottedList [Atom \"b\"] (Atom \"c\"),Atom \"d\"]"

    it "parseQuoted" $ do
      readExpr "'(a b)" `shouldBe` "Found value: List [Atom \"quote\",List [Atom \"a\",Atom \"b\"]]"
      readExpr "(a 'b c)" `shouldBe` "Found value: List [Atom \"a\",List [Atom \"quote\",Atom \"b\"],Atom \"c\"]"
