#!/usr/bin/env stack
{- stack
    --resolver lts-18.28
    script
    --package aeson
    --package aeson-qq
    --package hspec
    --package hspec-wai
    --package http-types
    --package tagged
    --package text
    --package unordered-containers
    --package wai
    --package wai-extra
    --package aeson-match-qq
-}
--
-- This example shows how to extract data from JSON documents using aeson-match-qq
-- with hspec-wai. It uses ~some~ a lot of type-fuckery to get a clean-ish syntax
-- for extractions, which is completely unnecessary in the real world but looks cool.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Aeson.Match.QQ (Matcher, match, qq)
import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ (aesonQQ)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Proxy (Proxy(..))
import           Data.String (fromString)
import           Data.Tagged (Tagged(..))
import           Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import           Network.HTTP.Types (ok200)
import           Network.Wai (Application, responseLBS)
import           Network.Wai.Test (SResponse, simpleBody)
import           Test.Hspec (Spec, hspec, it, shouldBe)
import           Test.Hspec.Wai


main :: IO ()
main =
  hspec spec

spec :: Spec
spec =
  with (pure app) $
    it "matches JSON and extracts data from it" $ do
      Get firstHobbyName <- get "/" `shouldMatchJson`
        ( 200
        , [qq|
            { "people":
              (unordered)
              [ { "name": "Drew"
                , "hobbies":
                  [ { "name": _firstHobbyName }
                  , ...
                  ]
                }
              , ...
              ]
            , ...
            }
          |]
        )
      let _ = firstHobbyName :: Tagged "firstHobbyName" _
      liftIO (firstHobbyName `shouldBe` Tagged "bridge")

shouldMatchJson
  :: WaiSession st SResponse
  -> (Int, Matcher Aeson.Value)
  -> WaiSession st (HashMap Text Aeson.Value)
action `shouldMatchJson` (code, matcher) = do
  res <- action
  pure res `shouldRespondWith` 200
    { matchStatus = code
    , matchBody = matchJson matcher
    }
  let Just val = Aeson.decode (simpleBody res)
      Right x = match matcher val
  pure x

matchJson :: Matcher Aeson.Value -> MatchBody
matchJson val = MatchBody matcher
 where
  matcher _headers body = do
    let Just json = Aeson.decode body
    case match val json of
      Left failures ->
        pure (pp failures)
      Right _ ->
        Nothing
   where
    pp failures =
      foldMap (\line -> line <> "\n") $
        fmap (Text.Lazy.unpack . Text.Lazy.decodeUtf8 . Aeson.encode) failures

pattern Get
  :: forall (key :: Symbol). KnownSymbol key => Tagged key Text -> HashMap Text Aeson.Value
pattern Get id <-
  (HashMap.lookup (fromString (symbolVal (Proxy @key))) -> Just (Aeson.String (Tagged -> id)))

app :: Application
app _req resf =
  resf (responseLBS ok200 [] (Aeson.encode value))

value :: Aeson.Value
value =
  [aesonQQ|
    { "foo": "Hello"
    , "bar": 1
    , "baz": "More stuff"
    , "people":
      [ { "name": "Drew"
        , "hobbies":
          [ { "name": "bridge" }
          , { "name": "haskell" }
          ]
        }
      , { "name": "Jane"
        , "hobbies":
          [ { "name": "chess" }
          , { "name": "ocaml" }
          ]
        }
      ]
    }
  |]
