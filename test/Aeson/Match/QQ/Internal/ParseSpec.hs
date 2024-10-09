{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Aeson.Match.QQ.Internal.ParseSpec (spec) where

import Data.Aeson qualified as Aeson
import Test.Hspec

import Aeson.Match.QQ


spec :: Spec
spec = do
  it "specs" $ do
    [qq| _ |] `shouldBe` Sig AnyT False (Var "")
    [qq| _hole |] `shouldBe` Sig AnyT False (Var "hole")
    [qq| _"fancy hole" |] `shouldBe` Sig AnyT False (Var "fancy hole")
    [qq| _typed-hole : number |] `shouldBe` Sig NumberT False (Var "typed-hole")
    [qq| _typed-?-hole : number? |] `shouldBe` Sig NumberT True (Var "typed-?-hole")
    [qq| _ : any |] `shouldBe` Sig AnyT False (Var "")

    [qq| null |] `shouldBe` Null

    [qq| false |] `shouldBe` Bool False
    [qq| true |] `shouldBe` Bool True

    [qq| 4 |] `shouldBe` Number 4
    [qq| -7 |] `shouldBe` Number (-7)

    [qq| "foo" |] `shouldBe` String "foo"

    [qq| [] |] `shouldBe`
      Array Box {values = [], extra = False}
    [qq| [1, 2, 3] |] `shouldBe`
      Array Box {values = [Number 1, Number 2, Number 3], extra = False}
    [qq| [1, _, 3] |] `shouldBe`
      Array Box {values = [Number 1, Sig AnyT False (Var ""), Number 3], extra = False}
    [qq| [1, _, 3, ...] |] `shouldBe`
      Array Box {values = [Number 1, Sig AnyT False (Var ""), Number 3], extra = True}

    [qq| (unordered) [] |] `shouldBe`
      ArrayUO Box {values = [], extra = False}
    [qq| (unordered) [1, 2, 3] |] `shouldBe`
      ArrayUO Box {values = [Number 1, Number 2, Number 3], extra = False}
    [qq| (unordered) [1, _, 3] |] `shouldBe`
      ArrayUO Box {values = [Number 1, Sig AnyT False (Var ""), Number 3], extra = False}
    [qq| (unordered) [1, _, 3, ...] |] `shouldBe`
      ArrayUO Box {values = [Number 1, Sig AnyT False (Var ""), Number 3], extra = True}

    [qq| {} |] `shouldBe`
      Object Box {values = [], extra = False}
    [qq| {foo: 4} |] `shouldBe`
      Object Box {values = [("foo", [Number 4])], extra = False}
    [qq| {foo: 4, "bar": 7} |] `shouldBe`
      Object Box {values = [("foo", [Number 4]), ("bar", [Number 7])], extra = False}
    [qq| {foo: 4, "bar": 7, ...} |] `shouldBe`
      Object Box {values = [("foo", [Number 4]), ("bar", [Number 7])], extra = True}
    [qq| {foo: 4, foo: _name, "bar": 7} |] `shouldBe`
      Object Box
        { values =
          [ ("foo", [Number 4, Sig AnyT False (Var "name")])
          , ("bar", [Number 7])
          ]
        , extra = False
        }

    [qq| {foo: #{4 + 7 :: Int}} |] `shouldBe`
      Object Box {values = [("foo", [Ext (Aeson.Number 11)])], extra = False}
    [qq| {foo: #{4 + 7 :: ToEncoding Int}} |] `shouldBe`
      Object Box {values = [("foo", [Ext (Aeson.Number 11)])], extra = False}

newtype ToEncoding a = ToEncoding { unToEncoding :: a }
    deriving (Show, Eq, Num)

instance Aeson.ToJSON a => Aeson.ToJSON (ToEncoding a) where
  toJSON =
    error "ToJSON is undefined"
  toEncoding =
    Aeson.toEncoding . unToEncoding
