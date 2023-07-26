{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Aeson.Match.QQ.Internal.PrettyPrintSpec (spec) where

import           Test.Hspec

import           Aeson.Match.QQ (qq)
import           Aeson.Match.QQ.Internal.PrettyPrint (pp)


spec :: Spec
spec = do
  it "holes" $ do
    pp [qq| _ |] `shouldBe`
      "[qq|\n\
      \  _ : any\n\
      \|]"
    pp [qq| _hole |] `shouldBe`
      "[qq|\n\
      \  _hole : any\n\
      \|]"
    pp [qq| _"fancy hole" |] `shouldBe`
      "[qq|\n\
      \  _\"fancy hole\" : any\n\
      \|]"
    pp [qq| _typed-hole : number |] `shouldBe`
      "[qq|\n\
      \  _typed-hole : number\n\
      \|]"
    pp [qq| _nullable-hole : string? |] `shouldBe`
      "[qq|\n\
      \  _nullable-hole : string?\n\
      \|]"

  it "basic values" $ do
    pp [qq| null |] `shouldBe`
      "[qq|\n\
      \  null\n\
      \|]"

    pp [qq| false |] `shouldBe`
      "[qq|\n\
      \  false\n\
      \|]"
    pp [qq| true |] `shouldBe`
      "[qq|\n\
      \  true\n\
      \|]"

    pp [qq| 4 |] `shouldBe`
      "[qq|\n\
      \  4\n\
      \|]"
    pp [qq| 7.42 |] `shouldBe`
      "[qq|\n\
      \  7.42\n\
      \|]"

    pp [qq| "" |] `shouldBe`
      "[qq|\n\
      \  \"\"\n\
      \|]"
    pp [qq| "foo" |] `shouldBe`
      "[qq|\n\
      \  \"foo\"\n\
      \|]"

    pp [qq| (ci) "" |] `shouldBe`
      "[qq|\n\
      \  (ci)\n\
      \  \"\"\n\
      \|]"
    pp [qq| (ci) "foo" |] `shouldBe`
      "[qq|\n\
      \  (ci)\n\
      \  \"foo\"\n\
      \|]"

  it "arrays" $ do
    pp [qq| [] |] `shouldBe`
      "[qq|\n\
      \  []\n\
      \|]"
    pp [qq| [1,2,3] |] `shouldBe`
      "[qq|\n\
      \  [ 1\n\
      \  , 2\n\
      \  , 3\n\
      \  ]\n\
      \|]"
    pp [qq| [1, ...] |] `shouldBe`
      "[qq|\n\
      \  [ 1\n\
      \  , ...\n\
      \  ]\n\
      \|]"
    pp [qq| [1, {qux: 42, quux: 0, ...}, 3] |] `shouldBe`
      "[qq|\n\
      \  [ 1\n\
      \  , { quux: 0\n\
      \    , qux: 42\n\
      \    , ...\n\
      \    }\n\
      \  , 3\n\
      \  ]\n\
      \|]"

    pp [qq| (unordered) [] |] `shouldBe`
      "[qq|\n\
      \  (unordered)\n\
      \  []\n\
      \|]"
    pp [qq| (unordered) [1,2,3] |] `shouldBe`
      "[qq|\n\
      \  (unordered)\n\
      \  [ 1\n\
      \  , 2\n\
      \  , 3\n\
      \  ]\n\
      \|]"

  it "objects" $ do
    pp [qq| {} |] `shouldBe`
      "[qq|\n\
      \  {}\n\
      \|]"
    pp [qq| {foo: 4, bar: 7} |] `shouldBe`
      "[qq|\n\
      \  { bar: 7\n\
      \  , foo: 4\n\
      \  }\n\
      \|]"
    pp [qq| {foo: 4, ...} |] `shouldBe`
      "[qq|\n\
      \  { foo: 4\n\
      \  , ...\n\
      \  }\n\
      \|]"
    pp [qq| {foo: 4, bar: {qux: 42, quux: 0, ...}, baz: 7} |] `shouldBe`
      "[qq|\n\
      \  { bar:\n\
      \    { quux: 0\n\
      \    , qux: 42\n\
      \    , ...\n\
      \    }\n\
      \  , baz: 7\n\
      \  , foo: 4\n\
      \  }\n\
      \|]"

    let
      spliced = 7 :: Int
    pp [qq| {foo: #{spliced}} |] `shouldBe`
      "[qq|\n\
      \  { foo: 7\n\
      \  }\n\
      \|]"
