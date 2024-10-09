{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Aeson.Match.QQ.Internal.PrettyPrint
  ( pp
  ) where

import Control.Monad ((<=<))
import Data.Aeson qualified as Aeson
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Char qualified as Char
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific, floatingOrInteger)
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text (Text)
import Data.Vector (Vector)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as PP

import Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
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
  Sig type_ nullable v ->
    rSig type_ nullable v
  Var name ->
    rVar name
  Ext ext ->
    rExt ext

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

rObject :: Box (HashMap Text (NonEmpty (Matcher Aeson.Value))) -> PP.Doc
rObject Box {values, extra} =
  case toKeyValues values of
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

toKeyValues :: (Ord k, Foldable t) => HashMap k (t v) -> [(k, v)]
toKeyValues =
  traverse toList <=< List.sortOn fst . HashMap.toList

rSig :: Type -> Bool -> Matcher Aeson.Value -> PP.Doc
rSig type_ nullable val =
  rValue val <+> ((":" <+> rType type_) <> bool PP.empty "?" nullable)
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

rVar :: Text -> PP.Doc
rVar name =
  "_" <> rName name

rName :: Text -> PP.Doc
rName name =
  PP.text (bool (Text.unpack name) (show name) (hasSpaces name))
 where
  hasSpaces =
    Text.any Char.isSpace

rExt :: Aeson.Value -> PP.Doc
rExt =
  fromString . Text.unpack . Text.decodeUtf8 . ByteString.Lazy.toStrict . Aeson.encode

simpleValue :: Matcher Aeson.Value -> Bool
simpleValue = \case
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
  Sig {} ->
    True
  Var {} ->
    True
  Ext {} ->
    True
