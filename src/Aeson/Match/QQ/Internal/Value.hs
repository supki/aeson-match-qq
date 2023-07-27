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
  , Type(..)
  , embed
  , quote
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Aeson (toHashMapText)
#endif
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Language.Haskell.TH (Q, Exp(..), Lit(..))
import           Language.Haskell.TH.Syntax (Lift(..))
import           Prelude hiding (any, null)
import qualified Text.PrettyPrint.HughesPJClass as PP (Pretty(..))

import           Aeson.Match.QQ.Internal.AesonUtils (toJSONE)


-- | A value constructed using 'qq' that attempts to match
-- a JSON document.
data Matcher ext
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | StringCI (CI Text)
    -- ^ Case-insensitive strings
  | Array (Array ext)
  | ArrayUO (Array ext)
    -- ^ Unordered arrays
  | Object (Object ext)
  | Var Text
    -- ^ Unless the name of the variable is '_', then
    -- the value is returned to the user in a 'HashMap'.
  | Sig Type Bool (Matcher ext)
  | Ext ext
    -- ^ External values spliced into a 'Matcher' using the `#{}` syntax
    deriving (Show, Eq, Functor)

type Array ext = Box (Vector (Matcher ext))

type Object ext = Box (HashMap Text (NonEmpty (Matcher ext)))

instance Aeson.ToJSON ext => Aeson.ToJSON (Matcher ext) where
  toJSON =
    Aeson.object . \case
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
      Var name ->
        [ "type" .= ("var" :: Text)
        , "name" .= name
        ]
      Sig type_ nullable v ->
        [ "type" .= ("sig" :: Text)
        , "expected-type" .= type_
        , "nullable" .= nullable
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

-- | It may be tempting to make this the 'Lift' instance for 'Matcher', but I don't
-- think it would be correct. We can get a lot from re-using 'Lift' machinery: namely,
-- we can cpmpletely bypass manual 'Exp' construction. But, fundamentally, 'Lift' is
-- for "serializing" Haskell values and it is not what we are attempting here.
quote :: Matcher Exp -> Q Exp
quote = \case
  Null ->
    [| Null :: Matcher Aeson.Value |]
  Bool b ->
    [| Bool b :: Matcher Aeson.Value |]
  Number n ->
    [| Number n :: Matcher Aeson.Value |]
  String str ->
    [| String str :: Matcher Aeson.Value |]
  StringCI ci -> let
      original = CI.original ci
    in
      [| StringCI (CI.mk original) :: Matcher Aeson.Value |]
  Array Box {values, extra} -> do
    let
      quoted =
        fmap ListE (traverse quote (Vector.toList values))
    [| Array Box
         { values = Vector.fromList $quoted
         , extra
         } :: Matcher Aeson.Value |]
  ArrayUO Box {values, extra} -> do
    let
      quoted =
        fmap ListE (traverse quote (Vector.toList values))
    [| ArrayUO Box
         { values = Vector.fromList $quoted
         , extra
         } :: Matcher Aeson.Value |]
  Object Box {values, extra} -> do
    let
      quoted =
        fmap toExp (traverse (traverse (traverse quote)) (HashMap.toList values))
      toExp =
        ListE . map (\(k, v) -> tup2 (LitE (StringL (Text.unpack k)), nonEmptyE v))
      tup2 (a, b) =
        TupE [Just a, Just b]
      nonEmptyE (e :| es) =
        AppE (AppE (ConE '(:|)) e) (ListE es)
    [| Object Box
         { values = HashMap.fromList $quoted
         , extra
         } :: Matcher Aeson.Value |]
  Sig type_ nullable val ->
    [| Sig type_ nullable $(quote val) :: Matcher Aeson.Value |]
  Var name ->
    [| Var name :: Matcher Aeson.Value |]
  -- | This is fundamentally type-unsafe as long as we try to splice `Exp` in.
  Ext ext ->
    [| Ext (toJSONE $(pure ext)) :: Matcher Aeson.Value |]

-- | _hole type
data Type
  = AnyT
    -- ^ @_ : any@ or, equivalently, @_@
  | BoolT
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
      AnyT {} -> "any" :: Text
      BoolT {} -> "bool"
      NumberT {} -> "number"
      StringT {} -> "string"
      StringCIT {} -> "ci-string"
      ArrayT {} -> "array"
      ArrayUOT {} -> "unordered-array"
      ObjectT {} -> "object"

instance PP.Pretty Type where
  pPrint = \case
    AnyT {} -> "any"
    BoolT {} -> "bool"
    NumberT {} -> "number"
    StringT {} -> "string"
    StringCIT {} -> "ci-string"
    ArrayT {} -> "array"
    ArrayUOT {} -> "unordered-array"
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
    Object Box {values = fmap (pure . embed) o, extra = False}
