{-# LANGUAGE OverloadedStrings #-}

module Badger.ParserSpec where

import Badger.Lang
import Badger.Parser

import Test.Hspec

spec :: Spec
spec = describe "Badger.Parser" $ do
  it "parses: val 3.0" $ do
    let t = "val 3.0"
    evaluate <$> parse t `shouldBe` (Right 3.0)