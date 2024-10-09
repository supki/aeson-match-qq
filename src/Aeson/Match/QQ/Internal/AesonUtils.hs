{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Aeson.Match.QQ.Internal.AesonUtils
  ( toJSONE
  , pp
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding.Internal qualified as Aeson (encodingToLazyByteString)
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.List qualified as List
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Scientific (Scientific, floatingOrInteger)
import Data.String (fromString)
import Data.Text (Text)
import Data.Vector (Vector)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as PP


-- | This is a round-about way to produce a 'Aeson.Value' from a 'ToJSON' instance.
-- it is written this way to avoid calling 'Aeson.toJSON' which might be undefined
-- for some datatypes that only implement 'toEncoding'.
--
-- It is defined in a separate module due to the TH stage restrictions as we need
-- to 'lift' 'toJSONE' eventually.
toJSONE :: Aeson.ToJSON x => x -> Aeson.Value
toJSONE x =
  let
    ~(Just val) = conv x
    -- ^ the pattern is irrefutable because we assume that it is always possible
    -- to recover a Value from an Encoding generated by Aeson.toEncoding
  in
    val
 where
  conv =
    Aeson.decode . Aeson.encodingToLazyByteString . Aeson.toEncoding

-- | A super-basic re-implementation of aeson-pretty. This function attains 2 goals:
--
--   - we avoid another dependency
--   - it uses the same prettyprinter everything else uses, and thus
--   it is easily integrated.
pp :: Aeson.Value -> PP.Doc
pp = \case
  Aeson.Null ->
    rNull
  Aeson.Bool b ->
    rBool b
  Aeson.Number n ->
    rNumber n
  Aeson.String str ->
    rString str
  Aeson.Array xs ->
    rArray xs
  Aeson.Object o ->
    rObject (Aeson.KeyMap.toHashMapText o)
 where
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

  rArray :: Vector Aeson.Value -> PP.Doc
  rArray values =
    case toList values of
      [] ->
        "[]"
      x : xs ->
        PP.vcat $
          ["[" <+> pp x] <>
          map (\x' -> "," <+> pp x') xs <>
          ["]"]

  rObject :: HashMap Text Aeson.Value -> PP.Doc
  rObject values =
    case List.sortOn fst (HashMap.toList values) of
      [] ->
        "{}"
      kv : kvs ->
        PP.vcat $
          ["{" <+> rKeyValue kv] <>
          map (\kv' -> "," <+> rKeyValue kv') kvs <>
          ["}"]
   where
    rKeyValue (key, value) =
      if simpleValue value then
        (rString key <> ":") <+> pp value
      else
        PP.vcat
          [ rString key <> ":"
          , pp value
          ]

  simpleValue :: Aeson.Value -> Bool
  simpleValue = \case
    Aeson.Null {} ->
      True
    Aeson.Bool {} ->
      True
    Aeson.Number {} ->
      True
    Aeson.String {} ->
      True
    Aeson.Array {} ->
      False
    Aeson.Object {} ->
      False
