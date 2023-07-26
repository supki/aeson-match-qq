{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Aeson.Match.QQ.Internal.PrettyPrint
  ( pp
  ) where

import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import           Data.Foldable (toList)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Int (Int64)
import qualified Data.List as List
import           Data.Scientific (Scientific, floatingOrInteger)
import           Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as PP

import           Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
  , HoleSig(..)
  , Type(..)
  , Box(..)
  )


pp :: Matcher Aeson.Value -> PP.Doc
pp value =
  PP.vcat
    [ "[qq|"
    , PP.nest 2 (rValue value)
    , "|]"
    ]

rValue :: Matcher Aeson.Value -> PP.Doc
rValue = \case
  Hole sig name ->
    rHole sig name
  Null ->
    rNull
  Bool b ->
    rBool b
  Number n ->
    rNumber n
  String str ->
    rString str
  StringCI str ->
    rStringCI str
  Array xs ->
    rArray xs
  ArrayUO xs ->
    rArrayUO xs
  Object o ->
    rObject o
  Ext ext ->
    rExt ext

rHole :: HoleSig -> Maybe Text -> PP.Doc
rHole sig name =
  ("_" <> maybe PP.empty rName name) <+> rSig sig

rName :: Text -> PP.Doc
rName name =
  PP.text (bool (Text.unpack name) (show name) (hasSpaces name))
 where
  hasSpaces =
    Text.any Char.isSpace

rSig :: HoleSig -> PP.Doc
rSig HoleSig {type_, nullable} =
  (":" <+> rType type_) <> bool PP.empty "?" nullable
 where
  rType = \case
    AnyT -> "any"
    BoolT -> "bool"
    NumberT -> "number"
    StringT -> "string"
    StringCIT -> "ci-string"
    ArrayT -> "array"
    ArrayUOT -> "unordered-array"
    ObjectT -> "object"

rNull :: PP.Doc
rNull =
  "null"

rBool :: Bool -> PP.Doc
rBool =
  bool "false" "true"

rNumber :: Scientific -> PP.Doc
rNumber =
  fromString . either (show @Double) (show @Int64) . floatingOrInteger

rString :: Text -> PP.Doc
rString =
  fromString . show

rStringCI :: CI Text -> PP.Doc
rStringCI str =
  PP.vcat
    [ "(ci)"
    , rString (CI.original str)
    ]

rArray :: Box (Vector (Matcher Aeson.Value)) -> PP.Doc
rArray Box {values, extra} =
  case toList values of
    [] ->
      "[]"
    x : xs ->
      PP.vcat $
        ["[" <+> rValue x] <>
        map (\x' -> "," <+> rValue x') xs <>
        [bool PP.empty ", ..." extra, "]"]

rArrayUO :: Box (Vector (Matcher Aeson.Value)) -> PP.Doc
rArrayUO box =
  PP.vcat
    [ "(unordered)"
    , rArray box
    ]

rExt :: Aeson.Value -> PP.Doc
rExt =
  fromString . Text.unpack . Text.decodeUtf8 . ByteString.Lazy.toStrict . Aeson.encode

rObject :: Box (HashMap Text (Matcher Aeson.Value)) -> PP.Doc
rObject Box {values, extra} =
  case List.sortOn fst (HashMap.toList values) of
    [] ->
      "{}"
    kv : kvs ->
      PP.vcat $
        ["{" <+> rKeyValue kv] <>
        map (\kv' -> "," <+> rKeyValue kv') kvs <>
        [bool PP.empty ", ..." extra, "}"]
 where
  rKeyValue (key, value) =
    if simpleValue value then
      (rName key <> ":") <+> rValue value
    else
      PP.vcat
        [ rName key <> ":"
        , rValue value
        ]

simpleValue :: Matcher Aeson.Value -> Bool
simpleValue = \case
  Hole {} ->
    True
  Null {} ->
    True
  Bool {} ->
    True
  Number {} ->
    True
  String {} ->
    True
  StringCI {} ->
    True
  Array {} ->
    False
  ArrayUO {} ->
    False
  Object {} ->
    False
  Ext {} ->
    True
