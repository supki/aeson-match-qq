{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Aeson.Match.QQSpec (spec) where

import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ (aesonQQ)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.HashMap.Strict as HashMap
import           Test.Hspec

import           Aeson.Match.QQ


spec :: Spec
spec = do
  describe "parse" $
    it "specs" $ do
      [qq| _ |] `shouldBe` Hole Nothing Nothing
      [qq| _hole |] `shouldBe` Hole Nothing (pure "hole")
      [qq| _"fancy hole" |] `shouldBe` Hole Nothing (pure "fancy hole")
      [qq| _typed-hole : number |] `shouldBe` Hole (pure (HoleSig NumberT False)) (pure "typed-hole")
      [qq| _typed-nullable-hole : number? |] `shouldBe` Hole (pure (HoleSig NumberT True)) (pure "typed-nullable-hole")

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
        Array Box {values = [Number 1, Hole Nothing Nothing, Number 3], extra = False}
      [qq| [1, _, 3, ...] |] `shouldBe`
        Array Box {values = [Number 1, Hole Nothing Nothing, Number 3], extra = True}

      [qq| (unordered) [] |] `shouldBe`
        ArrayUO Box {values = [], extra = False}
      [qq| (unordered) [1, 2, 3] |] `shouldBe`
        ArrayUO Box {values = [Number 1, Number 2, Number 3], extra = False}
      [qq| (unordered) [1, _, 3] |] `shouldBe`
        ArrayUO Box {values = [Number 1, Hole Nothing Nothing, Number 3], extra = False}
      [qq| (unordered) [1, _, 3, ...] |] `shouldBe`
        ArrayUO Box {values = [Number 1, Hole Nothing Nothing, Number 3], extra = True}

      [qq| {} |] `shouldBe`
        Object Box {values = [], extra = False}
      [qq| {foo: 4} |] `shouldBe`
        Object Box {values = [("foo", Number 4)], extra = False}
      [qq| {foo: 4, "bar": 7} |] `shouldBe`
        Object Box {values = [("foo", Number 4), ("bar", Number 7)], extra = False}
      [qq| {foo: 4, "bar": 7, ...} |] `shouldBe`
        Object Box {values = [("foo", Number 4), ("bar", Number 7)], extra = True}

      [qq| {foo: #{4 + 7 :: Int}} |] `shouldBe`
        Object Box {values = [("foo", Ext (Aeson.Number 11))], extra = False}
      [qq| {foo: #{4 + 7 :: ToEncoding Int}} |] `shouldBe`
        Object Box {values = [("foo", Ext (Aeson.Number 11))], extra = False}

  describe "match" $ do
    it "specs" $ do
      [qq| _ |] `shouldMatch` [aesonQQ| {foo: 4, bar: 7} |]
      [qq| null |] `shouldMatch` [aesonQQ| null |]
      [qq| true |] `shouldMatch` [aesonQQ| true |]
      [qq| false |] `shouldMatch` [aesonQQ| false |]
      [qq| 4 |] `shouldMatch` [aesonQQ| 4 |]
      [qq| "foo" |] `shouldMatch` [aesonQQ| "foo" |]
      [qq| (ci) "foo" |] `shouldMatch` [aesonQQ| "Foo" |]
      [qq| [1, 2, 3] |] `shouldMatch` [aesonQQ| [1, 2, 3] |]
      [qq| [1, _ : number, 3, ...] |] `shouldMatch` [aesonQQ| [1, 2, 3, 4] |]
      [qq| [1, _ : string] |] `shouldMatch` [aesonQQ| [1, "foo"] |]
      [qq| [1, _ : ci-string] |] `shouldMatch` [aesonQQ| [1, "foo"] |]
      [qq| [1, _ : unordered-array] |] `shouldMatch` [aesonQQ| [1, ["foo"]] |]
      [qq| (unordered) [] |] `shouldMatch` [aesonQQ| [] |]
      [qq| (unordered) [1, 2, 3] |] `shouldMatch` [aesonQQ| [1, 2, 3] |]
      [qq| (unordered) [1, 2, 3] |] `shouldMatch` [aesonQQ| [2, 3, 1] |]
      [qq| (unordered) [1, 2, 2] |] `shouldMatch` [aesonQQ| [2, 2, 1] |]
      [qq| (unordered) [1, _, 2] |] `shouldMatch` [aesonQQ| [2, 2, 1] |]
      [qq| (unordered) [1, 2, ...] |] `shouldMatch` [aesonQQ| [2, 3, 1] |]
      [qq| (unordered) [1, 2, ...] |] `shouldMatch` [aesonQQ| [2, 2, 1] |]
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
      [qq| (ci) "foo" |] `shouldNotMatch` [aesonQQ| "Bar" |]
      [qq| [1, 2, 3] |] `shouldNotMatch` [aesonQQ| [1, 2, 3, 4] |]
      [qq| [1, 2, 3, ...] |] `shouldNotMatch` [aesonQQ| [1, 2] |]
      [qq| [1, _ : string] |] `shouldNotMatch` [aesonQQ| [1, 2] |]
      [qq| [1, 2, 3, ...] |] `shouldNotMatch` [aesonQQ| [1, 2, 4] |]
      [qq| (unordered) [1, 2, 3] |] `shouldNotMatch` [aesonQQ| [1, 2, 4] |]
      [qq| (unordered) [1, 2, 3, 4] |] `shouldNotMatch` [aesonQQ| [1, 2, 3] |]
      [qq| (unordered) [1, 2] |] `shouldNotMatch` [aesonQQ| [1, 2, 2] |]
      [qq| {foo: 4, bar: 7} |] `shouldNotMatch` [aesonQQ| {foo: 7, bar: 4} |]
      [qq| {foo: 4, bar: 7} |] `shouldNotMatch` [aesonQQ| {foo: 4, baz: 7} |]
      [qq| {foo: 4, bar: 7, ...} |] `shouldNotMatch` [aesonQQ| {foo: 4, baz: 11} |]
      [qq| #{1 + 2 :: Int} |] `shouldNotMatch` [aesonQQ| 4 |]
      [qq| {foo: _ : number, bar: 7} |] `shouldNotMatch` [aesonQQ| {foo: "foo", bar: 7} |]
      [qq| {foo: _ : number, bar: 7} |] `shouldNotMatch` [aesonQQ| {foo: null, bar: 7} |]
      [qq|
        { foo: _ : string
        , bar: 7
        } |] `shouldNotMatch` [aesonQQ| {foo: null, bar: 7} |]

    it "paths" $ do
      match [qq| {foo: {bar: {baz: [1, 4]}}} |] [aesonQQ| {foo: {bar: {baz: [1, 7]}}} |] `shouldBe`
        throwE (Mismatch MkMismatch
          { path = ["foo", "bar", "baz", Idx 1]
          , matcher = [qq| 4 |]
          , given = [aesonQQ| 7 |]
          })

    context "named holes" $ do
      it "matches" $
        match [qq| {foo: _hole} |] [aesonQQ| {foo: {bar: {baz: [1, 4]}}} |] `shouldBe`
          pure (HashMap.singleton "hole" [aesonQQ| {bar: {baz: [1, 4]}} |])

      -- https://github.com/supki/aeson-match-qq/issues/26
      it "#26" $
        match [qq|
          { foo: _hole
          }
        |] [aesonQQ| {foo: {bar: {baz: [1, 4]}}} |] `shouldBe`
          pure (HashMap.singleton "hole" [aesonQQ| {bar: {baz: [1, 4]}} |])

    context "unordered array" $
      it "named holes" $ do
        match [qq| (unordered) [1, _hole] |] [aesonQQ| [2, 1] |] `shouldBe`
          pure (HashMap.singleton "hole" [aesonQQ| 2 |])
        match [qq| (unordered) [{foo: _hole}, ...] |] [aesonQQ| [{foo: 2}, 1] |] `shouldBe`
          pure (HashMap.singleton "hole" [aesonQQ| 2 |])

    -- https://github.com/supki/aeson-match-qq/issues/7
    it "#7" $ do
      match [qq| {foo: _} |] [aesonQQ| {} |] `shouldBe`
        throwE (MissingPathElem MkMissingPathElem
          { path = []
          , missing = "foo"
          })
      match [qq| [_] |] [aesonQQ| [] |] `shouldBe`
        throwE (MissingPathElem MkMissingPathElem
          { path = []
          , missing = Idx 0
          })

    -- https://github.com/supki/aeson-match-qq/issues/10
    it "#10" $ do
      [qq| {foo: []} |] `shouldBe`
        Object
          (Box
            { values = [("foo", Array (Box {values = [], extra = False}))]
            , extra = False
            })
      [qq| [{}] |] `shouldBe`
        Array
          (Box
            { values = [Object (Box {values = [], extra = False})]
            , extra = False
            })

    -- https://github.com/supki/aeson-match-qq/issues/12
    it "#12" $ do
      [qq| [ ... ] |] `shouldBe` [qq| _ : array |]
      [qq| (unordered) [ ... ] |] `shouldBe` [qq| _ : unordered-array |]
      [qq| { ... } |] `shouldBe` [qq| _ : object |]

    -- https://github.com/supki/aeson-match-qq/issues/13
    it "#13" $
      [qq| "Слава Україні" |] `shouldMatch` [aesonQQ| "Слава Україні" |]

    -- https://github.com/supki/aeson-match-qq/issues/18
    it "#18" $ do
      -- string ~ string
      match [qq| "foo" |] [aesonQQ| "bar" |] `shouldBe`
        throwE (Mismatch MkMismatch
          { path = []
          , matcher = String "foo"
          , given = Aeson.String "bar"
          })
      -- string !~ number
      match [qq| "foo" |] [aesonQQ| 4 |] `shouldBe`
        throwE (Mistype MkMismatch
          { path = []
          , matcher = String "foo"
          , given = Aeson.Number 4
          })
      -- string !~ null
      match [qq| "foo" |] [aesonQQ| null |] `shouldBe`
        throwE (Mistype MkMismatch
          { path = []
          , matcher = String "foo"
          , given = Aeson.Null
          })
      -- null !~ number
      match [qq| null |] [aesonQQ| 4 |] `shouldBe`
        throwE (Mistype MkMismatch
          { path = []
          , matcher = Null
          , given = Aeson.Number 4
          })

    -- https://github.com/supki/aeson-match-qq/issues/29
    it "#29" $ do
      parse "_ : not-a-known-type" `shouldBe`
        Left "unknown type in hole signature: Failed reading: empty"

    -- https://github.com/supki/aeson-match-qq/issues/32
    it "#32" $ do
      parse "null some garbage" `shouldBe`
        Left "trailing garbage after a Matcher value: endOfInput"

    it "pretty" $ do
      prettyError (Mismatch MkMismatch
        { path = [Key "foo", Idx 0, Key "bar"]
        , matcher = String "foo"
        , given = Aeson.String "bar"
        }) `shouldBe`
          "  error: value does not match\n\
          \   path: .foo[0].bar\n\
          \matcher: {\"value\":\"foo\",\"type\":\"string\"}\n\
          \  given: \"bar\""
      prettyError (Mistype MkMismatch
        { path = [Key "foo", Idx 0, Key "bar"]
        , matcher = String "foo"
        , given = Aeson.Number 4
        }) `shouldBe`
          "  error: type of value does not match\n\
          \   path: .foo[0].bar\n\
          \matcher: {\"value\":\"foo\",\"type\":\"string\"}\n\
          \  given: 4"
      prettyError (MissingPathElem MkMissingPathElem
        { path = [Key "foo", Idx 0, Key "bar"]
        , missing = Idx 1
        }) `shouldBe`
          "  error: missing key or index\n\
          \   path: .foo[0].bar\n\
          \missing: [1]"
      prettyError (ExtraArrayValues MkExtraArrayValues
        { path = [Key "foo", Idx 0, Key "bar"]
        , values = [Aeson.String "foo", Aeson.Number 4]
        }) `shouldBe`
          "  error: extra array values\n\
          \   path: .foo[0].bar\n\
          \ values: \"foo\"\n\
          \         4"
      prettyError (ExtraObjectValues MkExtraObjectValues
        { path = [Key "foo", Idx 0, Key "bar"]
        , values = HashMap.fromList [("k0", Aeson.String "foo"), ("k1", Aeson.Number 4)]
        }) `shouldBe`
          "  error: extra object values\n\
          \   path: .foo[0].bar\n\
          \ values:   key: .k0\n\
          \         value: \"foo\"\n\
          \           key: .k1\n\
          \         value: 4"

newtype ToEncoding a = ToEncoding { unToEncoding :: a }
    deriving (Show, Eq, Num)

instance Aeson.ToJSON a => Aeson.ToJSON (ToEncoding a) where
  toJSON =
    error "ToJSON is undefined"
  toEncoding =
    Aeson.toEncoding . unToEncoding

shouldMatch :: HasCallStack => Matcher Aeson.Value -> Aeson.Value -> Expectation
shouldMatch a b =
  match a b `shouldBe` pure mempty

shouldNotMatch :: HasCallStack => Matcher Aeson.Value -> Aeson.Value -> Expectation
shouldNotMatch a b =
  match a b `shouldNotBe` pure mempty

throwE :: e -> Either (NonEmpty e) a
throwE =
  Left . pure
