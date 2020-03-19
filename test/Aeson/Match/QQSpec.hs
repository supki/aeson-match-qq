{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Aeson.Match.QQSpec (spec) where

import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.HashMap.Strict as HashMap
import           Test.Hspec

import           Aeson.Match.QQ


spec :: Spec
spec = do
  describe "parse" $
    it "specs" $ do
      [qq| _ |] `shouldBe` Any Nothing Nothing
      [qq| _hole |] `shouldBe` Any Nothing (pure "hole")
      [qq| _"fancy hole" |] `shouldBe` Any Nothing (pure "fancy hole")
      [qq| _typed-hole : number |] `shouldBe` Any (pure (TypeSig NumberT NonNullable)) (pure "typed-hole")
      [qq| _typed-nullable-hole : number? |] `shouldBe` Any (pure (TypeSig NumberT Nullable)) (pure "typed-nullable-hole")

      [qq| null |] `shouldBe` Null

      [qq| false |] `shouldBe` Bool False
      [qq| true |] `shouldBe` Bool True

      [qq| 4 |] `shouldBe` Number 4
      [qq| -7 |] `shouldBe` Number (-7)

      [qq| "foo" |] `shouldBe` String "foo"

      [qq| [] |] `shouldBe`
        Array Box {knownValues = [], extendable = False}
      [qq| [1, 2, 3] |] `shouldBe`
        Array Box {knownValues = [Number 1, Number 2, Number 3], extendable = False}
      [qq| [1, _, 3] |] `shouldBe`
        Array Box {knownValues = [Number 1, Any Nothing Nothing, Number 3], extendable = False}
      [qq| [1, _, 3, ...] |] `shouldBe`
        Array Box {knownValues = [Number 1, Any Nothing Nothing, Number 3], extendable = True}

      [qq| {} |] `shouldBe`
        Object Box {knownValues = [], extendable = False}
      [qq| {foo: 4} |] `shouldBe`
        Object Box {knownValues = [("foo", Number 4)], extendable = False}
      [qq| {foo: 4, "bar": 7} |] `shouldBe`
        Object Box {knownValues = [("foo", Number 4), ("bar", Number 7)], extendable = False}
      [qq| {foo: 4, "bar": 7, ...} |] `shouldBe`
        Object Box {knownValues = [("foo", Number 4), ("bar", Number 7)], extendable = True}

      [qq| {foo: #{4 + 7 :: Int}} |] `shouldBe`
        Object Box {knownValues = [("foo", Ext (Aeson.Number 11))], extendable = False}
      [qq| {foo: #{4 + 7 :: ToEncoding Int}} |] `shouldBe`
        Object Box {knownValues = [("foo", Ext (Aeson.Number 11))], extendable = False}

  describe "match" $ do
    it "specs" $ do
      [qq| _ |] `shouldMatch` [aesonQQ| {foo: 4, bar: 7} |]
      [qq| null |] `shouldMatch` [aesonQQ| null |]
      [qq| true |] `shouldMatch` [aesonQQ| true |]
      [qq| false |] `shouldMatch` [aesonQQ| false |]
      [qq| 4 |] `shouldMatch` [aesonQQ| 4 |]
      [qq| "foo" |] `shouldMatch` [aesonQQ| "foo" |]
      [qq| [1, 2, 3] |] `shouldMatch` [aesonQQ| [1, 2, 3] |]
      [qq| [1, _ : number, 3, ...] |] `shouldMatch` [aesonQQ| [1, 2, 3, 4] |]
      [qq| [1, _ : string] |] `shouldMatch` [aesonQQ| [1, "foo"] |]
      [qq| {foo: 4, bar: 7} |] `shouldMatch` [aesonQQ| {foo: 4, bar: 7} |]
      [qq| {foo: 4, bar: 7, ...} |] `shouldMatch` [aesonQQ| {foo: 4, bar: 7, baz: 11} |]
      [qq| #{1 + 2 :: Int} |] `shouldMatch` [aesonQQ| 3 |]
      [qq| {foo: _ : number, bar: 7} |] `shouldMatch` [aesonQQ| {foo: 4, bar: 7} |]
      [qq| {foo: _ : number?, bar: 7} |] `shouldMatch` [aesonQQ| {foo: null, bar: 7} |]

      [qq| null |] `shouldNotMatch` [aesonQQ| 4 |]
      [qq| true |] `shouldNotMatch` [aesonQQ| false |]
      [qq| false |] `shouldNotMatch` [aesonQQ| true |]
      [qq| 4 |] `shouldNotMatch` [aesonQQ| 7 |]
      [qq| "foo" |] `shouldNotMatch` [aesonQQ| "bar" |]
      [qq| [1, 2, 3] |] `shouldNotMatch` [aesonQQ| [1, 2, 3, 4] |]
      [qq| [1, 2, 3, ...] |] `shouldNotMatch` [aesonQQ| [1, 2] |]
      [qq| [1, _ : string] |] `shouldNotMatch` [aesonQQ| [1, 2] |]
      [qq| [1, 2, 3, ...] |] `shouldNotMatch` [aesonQQ| [1, 2, 4] |]
      [qq| {foo: 4, bar: 7} |] `shouldNotMatch` [aesonQQ| {foo: 7, bar: 4} |]
      [qq| {foo: 4, bar: 7} |] `shouldNotMatch` [aesonQQ| {foo: 4, baz: 7} |]
      [qq| {foo: 4, bar: 7, ...} |] `shouldNotMatch` [aesonQQ| {foo: 4, baz: 11} |]
      [qq| #{1 + 2 :: Int} |] `shouldNotMatch` [aesonQQ| 4 |]
      [qq| {foo: _ : number, bar: 7} |] `shouldNotMatch` [aesonQQ| {foo: "foo", bar: 7} |]
      [qq| {foo: _ : number, bar: 7} |] `shouldNotMatch` [aesonQQ| {foo: null, bar: 7} |]

    it "paths" $ do
      match [qq| {foo: {bar: {baz: [1, 4]}}} |] [aesonQQ| {foo: {bar: {baz: [1, 7]}}} |] `shouldBe`
        mismatch ["foo", "bar", "baz", Idx 1] [qq| 4 |] [aesonQQ| 7 |]

    it "named holes" $ do
      match [qq| {foo: _hole} |] [aesonQQ| {foo: {bar: {baz: [1, 4]}}} |] `shouldBe`
        pure (HashMap.singleton "hole" [aesonQQ| {bar: {baz: [1, 4]}} |])

    -- https://github.com/supki/aeson-match-qq/issues/7
    it "#7" $ do
      match [qq| {foo: _} |] [aesonQQ| {} |] `shouldBe`
        missingPathElem [] "foo"
      match [qq| [_] |] [aesonQQ| [] |] `shouldBe`
        missingPathElem [] (Idx 0)

    -- https://github.com/supki/aeson-match-qq/issues/10
    it "#10" $ do
      [qq| {foo: []} |] `shouldBe`
        Object
          (Box
            { knownValues = [("foo", Array (Box {knownValues = [], extendable = False}))]
            , extendable = False
            })
      [qq| [{}] |] `shouldBe`
        Array
          (Box
            { knownValues = [Object (Box {knownValues = [], extendable = False})]
            , extendable = False
            })

newtype ToEncoding a = ToEncoding { unToEncoding :: a }
    deriving (Show, Eq, Num)

instance Aeson.ToJSON a => Aeson.ToJSON (ToEncoding a) where
  toJSON =
    error "ToJSON is undefined"
  toEncoding =
    Aeson.toEncoding . unToEncoding

shouldMatch :: Value Aeson.Value -> Aeson.Value -> Expectation
shouldMatch a b =
  match a b `shouldBe` pure mempty

shouldNotMatch :: Value Aeson.Value -> Aeson.Value -> Expectation
shouldNotMatch a b =
  match a b `shouldNotBe` pure mempty
