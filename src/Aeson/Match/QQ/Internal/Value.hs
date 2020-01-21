{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Aeson.Match.QQ.Internal.Value
  ( Value(..)
  , Box(..)
  , Array
  , Object
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson (encodingToLazyByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Scientific (Scientific)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Language.Haskell.TH (Exp(..), Lit(..))
import           Language.Haskell.TH.Syntax (Lift(..))
import           Prelude hiding (any, null)


data Value ext
  = Any (Maybe Text)
  | Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Array (Array ext)
  | Object (Object ext)
  | Ext ext
    deriving (Show, Eq)

instance Aeson.ToJSON ext => Aeson.ToJSON (Value ext) where
  toJSON =
    Aeson.object . \case
      Any name ->
        [ "type" .= ("any" :: Text)
        , "name" .= name
        ]
      Null ->
        [ "type" .= ("null" :: Text)
        ]
      Bool v ->
        [ "type" .= ("bool" :: Text)
        , "value" .= v
        ]
      Number v ->
        [ "type" .= ("number" :: Text)
        , "value" .= v
        ]
      String v ->
        [ "type" .= ("string" :: Text)
        , "value" .= v
        ]
      Array v ->
        [ "type" .= ("array" :: Text)
        , "value" .= v
        ]
      Object v ->
        [ "type" .= ("object" :: Text)
        , "value" .= v
        ]
      Ext v ->
        [ "type" .= ("extension" :: Text)
        , "value" .= v
        ]

data Box a = Box
  { knownValues :: a
  , extendable  :: Bool
  } deriving (Show, Eq)

instance Aeson.ToJSON a => Aeson.ToJSON (Box a) where
  toJSON Box {..} =
    Aeson.object
      [ "known-values" .= knownValues
      , "extendable" .= extendable
      ]

type Array ext = Box (Vector (Value ext))

type Object ext = Box (HashMap Text (Value ext))

-- | Convert `Value Exp` to `Value Aeson.Value`. This uses a roundabout way to get
-- `Aeson.Value` from `ToJSON.toEncoding` to avoid calling `Aeson.toJSON` which may be
-- undefined for some datatypes.
instance ext ~ Exp => Lift (Value ext) where
  lift = \case
    Any name ->
      [| Any $(pure (maybe (ConE 'Nothing) (AppE (ConE 'Just) . AppE (VarE 'fromString) . LitE . textL) name)) :: Value Aeson.Value |]
    Null ->
      [| Null :: Value Aeson.Value |]
    Bool b ->
      [| Bool b :: Value Aeson.Value |]
    Number n ->
      [| Number (fromRational $(pure (LitE (RationalL (toRational n))))) :: Value Aeson.Value |]
    String str ->
      [| String (fromString $(pure (LitE (textL str)))) :: Value Aeson.Value |]
    Array Box {knownValues, extendable} -> [|
        Array Box
          { knownValues =
              Vector.fromList $(fmap (ListE . Vector.toList) (traverse lift knownValues))
          , extendable
          } :: Value Aeson.Value
      |]
    Object Box {knownValues, extendable} -> [|
        Object Box
          { knownValues =
              HashMap.fromList $(fmap (ListE . map (\(k, v) -> TupE [LitE (textL k), v]) . HashMap.toList) (traverse lift knownValues))
          , extendable
          } :: Value Aeson.Value
      |]
    Ext ext ->
      [| Ext (let Just val = Aeson.decode (Aeson.encodingToLazyByteString (Aeson.toEncoding $(pure ext))) in val) :: Value Aeson.Value |]
   where
    textL =
      StringL . Text.unpack
