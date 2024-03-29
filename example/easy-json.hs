#!/usr/bin/env stack
{- stack
    --resolver lts-18.28
    script
    --package aeson
    --package aeson-qq
    --package unordered-containers
    --package aeson-match-qq
-}
--
-- This example shows how to extract some data from a nested JSON value.
--
-- Inspired by https://blog.drewolson.org/easy-json
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Aeson.Match.QQ (Matcher, match, qq)
import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.HashMap.Strict as HashMap


main :: IO ()
main =
  case match matcher value of
    Right matched
      | Just (Aeson.String hobby) <- HashMap.lookup "firstHobbyName" matched ->
        print hobby
    _ ->
      error "sometimes, matchers fail"

matcher :: Matcher Aeson.Value
matcher =
  [qq|
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
