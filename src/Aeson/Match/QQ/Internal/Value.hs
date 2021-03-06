{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Aeson.Match.QQ.Internal.Value
  ( Value(..)
  , Box(..)
  , Array
  , Object
  , TypeSig(..)
  , Type(..)
  , Nullable(..)
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
  = Any (Maybe TypeSig) (Maybe Text)
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
      Any type_ name ->
        [ "type" .= ("any" :: Text)
        , "expected-type" .= type_
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
    Any type_ name ->
      [| Any type_ $(pure (maybe (ConE 'Nothing) (AppE (ConE 'Just) . AppE (VarE 'fromString) . LitE . textL) name)) :: Value Aeson.Value |]
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

data TypeSig = TypeSig
  { type_    :: Type
  , nullable :: Nullable
  } deriving (Show, Eq, Lift)

instance Aeson.ToJSON TypeSig where
  toJSON TypeSig {..} =
    Aeson.object
      [ "type" .= type_
      , "nullable" .= nullable
      ]

data Type
  = BoolT
  | NumberT
  | StringT
  | ArrayT
  | ObjectT
    deriving (Show, Eq, Lift)

instance Aeson.ToJSON Type where
  toJSON =
    Aeson.toJSON . \case
      BoolT {} -> "bool" :: Text
      NumberT {} -> "number"
      StringT {} -> "string"
      ArrayT {} -> "array"
      ObjectT {} -> "object"

data Nullable
  = Nullable
  | NonNullable
    deriving (Show, Eq, Lift)

instance Aeson.ToJSON Nullable where
  toJSON =
    Aeson.toJSON . \case
      Nullable -> True
      NonNullable -> False
