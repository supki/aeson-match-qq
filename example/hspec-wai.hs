#!/usr/bin/env stack
{- stack
    --resolver lts-18.28
    script
    --package aeson
    --package aeson-qq
    --package hspec
    --package hspec-wai
    --package http-types
    --package text
    --package wai
    --package aeson-match-qq
-}
--
-- This example shows how to integrate aeson-match-qq with hspec-wai.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Aeson.Match.QQ (qq)
import qualified Aeson.Match.QQ as Match
import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import           Network.HTTP.Types (ok200)
import           Network.Wai (Application, responseLBS)
import           Test.Hspec (Spec, hspec, it)
import           Test.Hspec.Wai


main :: IO ()
main =
  hspec spec

spec :: Spec
spec =
  with (pure app) $
    it "matches JSON" $
      get "/" `shouldRespondWith` 200
        { matchBody = matchJson [qq|
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
        }

matchJson :: Match.Value Aeson.Value -> MatchBody
matchJson val = MatchBody matcher
 where
  matcher _headers body = do
    let Just json = Aeson.decode body
    case Match.match val json of
      Left failures ->
        pure (pp failures)
      Right _ ->
        Nothing
   where
    pp failures =
      foldMap (\line -> line <> "\n") $
        fmap (Text.Lazy.unpack . Text.Lazy.decodeUtf8 . Aeson.encode) failures

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
