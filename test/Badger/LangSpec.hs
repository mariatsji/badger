module Badger.LangSpec where

import Test.Hspec

import Badger.Lang

spec :: Spec
spec = describe "Badger.Lang" $ do
  it "Creates a val and evaluates it" $ do
    let d = 232.0
        e = Val d
    evaluate e `shouldBe` d
  it "Creates a Sum of two expressions and evaluates it" $ do
    let d1 = 3.0
        d2 = -2
        e = Sum (Val d1) (Val d2)
    evaluate e `shouldBe` (d1 + d2)
