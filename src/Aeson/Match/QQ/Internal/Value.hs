{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
  , Box(..)
  , Array
  , Object
  , HoleSig(..)
  , Type(..)
  , embed
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Aeson (toHashMapText)
#endif
import qualified Data.Aeson.Encoding.Internal as Aeson (encodingToLazyByteString)
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Scientific (Scientific)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Language.Haskell.TH (Exp(..), Lit(..))
import           Language.Haskell.TH.Syntax
  ( Lift(..)
#if MIN_VERSION_template_haskell(2,17,0)
  , unsafeCodeCoerce
#else
  , unsafeTExpCoerce
#endif
  )
import           Prelude hiding (any, null)


-- | A value constructed using 'qq' that attempts to match
-- a JSON document.
data Matcher ext
  = Hole (Maybe HoleSig) (Maybe Text)
    -- ^ Optionally typed, optionally named _hole.
    -- If a type is provided, the _hole only matches those values
    -- that have that type.
    -- If a name is provided, the matched value is returned
    -- to the user.
  | Null
  | Bool Bool
  | Number Scientific
  | String Text
  | StringCI (CI Text)
    -- ^ Case-insensitive strings
  | Array (Array ext)
  | ArrayUO (Array ext)
    -- ^ Unordered arrays
  | Object (Object ext)
  | Ext ext
    -- ^ External values spliced into a 'Matcher' using the `#{}` syntax
    deriving (Show, Eq, Functor)

instance Aeson.ToJSON ext => Aeson.ToJSON (Matcher ext) where
  toJSON =
    Aeson.object . \case
      Hole type_ name ->
        [ "type" .= ("hole" :: Text)
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
      StringCI v ->
        [ "type" .= ("string-ci" :: Text)
        , "value" .= CI.original v
        ]
      Array v ->
        [ "type" .= ("array" :: Text)
        , "value" .= v
        ]
      ArrayUO v ->
        [ "type" .= ("array-unordered" :: Text)
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

-- | A wrapper for those matchers that support the `...` syntax.
data Box a = Box
  { values :: a
  , extra  :: Bool
    -- ^ Are extra, not specifically mentioned by a 'Matcher', values
    -- allowed in a 'Value'?
  } deriving (Show, Eq, Functor)

instance Aeson.ToJSON a => Aeson.ToJSON (Box a) where
  toJSON Box {..} =
    Aeson.object
      [ "values" .= values
      , "extra" .= extra
      ]

type Array ext = Box (Vector (Matcher ext))

type Object ext = Box (HashMap Text (Matcher ext))

-- | Convert `'Matcher' 'Exp'` to `'Matcher' 'Aeson.Value'`. This uses a roundabout way to get
-- `Aeson.Value` from `ToJSON.toEncoding` to avoid calling `Aeson.toJSON` which may be
-- undefined for some datatypes.
instance ext ~ Exp => Lift (Matcher ext) where
  lift = \case
    Hole type_ name ->
      [| Hole type_ $(pure (maybe (ConE 'Nothing) (AppE (ConE 'Just) . AppE (VarE 'fromString) . LitE . textL) name)) :: Matcher Aeson.Value |]
    Null ->
      [| Null :: Matcher Aeson.Value |]
    Bool b ->
      [| Bool b :: Matcher Aeson.Value |]
    Number n ->
      [| Number (fromRational $(pure (LitE (RationalL (toRational n))))) :: Matcher Aeson.Value |]
    String str ->
      [| String (fromString $(pure (LitE (textL str)))) :: Matcher Aeson.Value |]
    StringCI str ->
      [| StringCI (fromString $(pure (LitE (textL (CI.original str))))) :: Matcher Aeson.Value |]
    Array Box {values, extra} -> [|
        Array Box
          { values =
              Vector.fromList $(fmap (ListE . Vector.toList) (traverse lift values))
          , extra
          } :: Matcher Aeson.Value
      |]
    ArrayUO Box {values, extra} -> [|
        ArrayUO Box
          { values =
              Vector.fromList $(fmap (ListE . Vector.toList) (traverse lift values))
          , extra
          } :: Matcher Aeson.Value
      |]
    Object Box {values, extra} -> [|
        Object Box
          { values =
              HashMap.fromList $(fmap (ListE . map (\(k, v) -> TupE [Just (LitE (textL k)), Just v]) . HashMap.toList) (traverse lift values))
          , extra
          } :: Matcher Aeson.Value
      |]
    Ext ext ->
      [| Ext (let ~(Just val) = Aeson.decode (Aeson.encodingToLazyByteString (Aeson.toEncoding $(pure ext))) in val) :: Matcher Aeson.Value |]
   where
    textL =
      StringL . Text.unpack
  liftTyped =
#if MIN_VERSION_template_haskell(2,17,0)
    unsafeCodeCoerce . lift
#else
    unsafeTExpCoerce . lift
#endif

-- | _hole type signature
data HoleSig = HoleSig
  { type_    :: Type
  , nullable :: Bool
  } deriving (Show, Eq, Lift)

instance Aeson.ToJSON HoleSig where
  toJSON HoleSig {..} =
    Aeson.object
      [ "type" .= type_
      , "nullable" .= nullable
      ]

-- | _hole type
data Type
  = BoolT
    -- ^ @_ : bool@
  | NumberT
    -- ^ @_ : number@
  | StringT
    -- ^ @_ : string@
  | StringCIT
    -- ^ @_ : ci-string@
  | ArrayT
    -- ^ @_ : array@
  | ArrayUOT
    -- ^ @_ : unordered-array@
  | ObjectT
    -- ^ @_ : object@
    deriving (Show, Eq, Lift)

instance Aeson.ToJSON Type where
  toJSON =
    Aeson.toJSON . \case
      BoolT {} -> "bool" :: Text
      NumberT {} -> "number"
      StringT {} -> "string"
      StringCIT {} -> "ci-string"
      ArrayT {} -> "array"
      ArrayUOT {} -> "array-unordered"
      ObjectT {} -> "object"

embed :: Aeson.Value -> Matcher ext
embed = \case
  Aeson.Null ->
    Null
  Aeson.Bool b ->
    Bool b
  Aeson.Number n ->
    Number n
  Aeson.String n ->
    String n
  Aeson.Array xs ->
    Array Box {values = fmap embed xs, extra = False}
#if MIN_VERSION_aeson(2,0,0)
  Aeson.Object (Aeson.toHashMapText -> o) ->
#else
  Aeson.Object o ->
#endif
    Object Box {values = fmap embed o, extra = False}
