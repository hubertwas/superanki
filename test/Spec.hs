{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import SuperAnki (base62, parseCloze, parseDeletion, parseText, parseBase62, parseFile, Cloze(..), ClozeText(..))
import Numeric.Natural (Natural)
import Control.Lens (preview, review)
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "base62" $
    prop "is a prism" $ \n ->
      preview base62 (review base62 (n :: Natural)) == Just n

  describe "parseDeletion" $ do
    it "parses braces" $
      parseMaybe parseDeletion "{word}" `shouldBe` Just (Deletion "word")
    it "parses spaces inside braces" $
      parseMaybe parseDeletion "{multi word}" `shouldBe` Just (Deletion "multi word")
    it "rejects empty braces" $
      parseMaybe parseDeletion "{}" `shouldBe` Nothing

  describe "parseText" $ do
    it "parses plain text" $
      parseMaybe parseText "plain text" `shouldBe` Just (Text "plain text")
    it "rejects empty input" $
      parseMaybe parseText "" `shouldBe` Nothing

  describe "parseBase62" $ do
    it "parses mixed case and digits" $
      parseMaybe parseBase62 "abc123XYZ" `shouldBe` Just "abc123XYZ"
    it "rejects empty input" $
      parseMaybe parseBase62 "" `shouldBe` Nothing

  describe "parseCloze" $ do
    it "parses without id" $
      parseMaybe parseCloze "C: hello {world}"
        `shouldBe` Just (MkCloze Nothing [Text " hello ", Deletion "world"])
    it "parses with base62 id" $
      parseMaybe parseCloze "C1a6: hello {world}"
        `shouldBe` Just (MkCloze (Just 4470) [Text " hello ", Deletion "world"])
    it "parses empty body" $
      parseMaybe parseCloze "C: " `shouldBe` Just (MkCloze Nothing [Text " "])
    it "parses with leading whitespace" $
      parseMaybe parseCloze "  C: indented"
        `shouldBe` Just (MkCloze Nothing [Text " indented"])
    it "parses adjacent deletions" $
      parseMaybe parseCloze "C: {a}{b}"
        `shouldBe` Just (MkCloze Nothing [Text " ", Deletion "a", Deletion "b"])
    it "parses text only" $
      parseMaybe parseCloze "C: no deletions"
        `shouldBe` Just (MkCloze Nothing [Text " no deletions"])
    it "rejects missing colon" $
      parseMaybe parseCloze "C" `shouldBe` Nothing
    it "rejects non-cloze text" $
      parseMaybe parseCloze "not a cloze" `shouldBe` Nothing

  describe "parseFile" $ do
    it "parses mix of cloze and non-cloze lines" $
      parseMaybe parseFile "C: hello {world}\nskip this\nC: {another}\n"
        `shouldBe` Just [ MkCloze Nothing [Text " hello ", Deletion "world"]
                        , MkCloze Nothing [Text " ", Deletion "another"]
                        ]
    it "parses empty input" $
      parseMaybe parseFile "" `shouldBe` Just []

main :: IO ()
main = hspec spec
