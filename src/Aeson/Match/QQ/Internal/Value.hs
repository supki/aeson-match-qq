{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
  , embed
  , quote
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson (toHashMapText)
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Language.Haskell.TH (Q, Exp(..), Lit(..))
import Prelude hiding (any, exp, null)

import Aeson.Match.QQ.Internal.AesonUtils (toJSONE)
import Aeson.Match.QQ.Internal.Box (Box(..), Array, Object)
import Aeson.Match.QQ.Internal.Parse (Parsed)
import Aeson.Match.QQ.Internal.Parse qualified as Parsed
  ( Parsed(..)
  , CI(..)
  , StringFrag(..)
  )
import Aeson.Match.QQ.Internal.Type (Type)


-- | A value constructed using 'qq' that attempts to match
-- a JSON document.
data Matcher
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | StringCI (CI Text)
    -- ^ Case-insensitive strings
  | Array (Array Matcher)
  | ArrayUO (Array Matcher)
    -- ^ Unordered arrays
  | Object (Object Matcher)
  | Var Text
    -- ^ Unless the name of the variable is '_', then
    -- the value is returned to the user in a 'HashMap'.
  | Sig Type Bool Matcher
  | Ext Aeson.Value
    -- ^ External values spliced into a 'Matcher' using the `#{}` syntax
    deriving (Show, Eq)

-- | It may be tempting to make this the 'Lift' instance for 'Matcher', but I don't
-- think it would be correct. We can get a lot from re-using 'Lift' machinery: namely,
-- we can cpmpletely bypass manual 'Exp' construction. But, fundamentally, 'Lift' is
-- for "serializing" Haskell values and it is not what we are attempting here.
quote :: Parsed -> Q Exp
quote = \case
  Parsed.Null ->
    [| Null |]

  Parsed.Bool b ->
    [| Bool b |]

  Parsed.Number n ->
    [| Number n |]

  Parsed.String ci str -> do
    let
      fromFrag = \case
        Parsed.Raw lit ->
          [| lit |]
        Parsed.Interpolate exp ->
          [| $(pure exp) |]
      exps =
        fmap ListE (traverse fromFrag (toList str)) :: Q Exp
    case ci of
      Parsed.CS ->
        [| String (mconcat $exps) |]
      Parsed.CI ->
        [| StringCI (CI.mk (mconcat $exps)) |]

  Parsed.Array Box {values, extra} -> do
    let
      quoted =
        fmap ListE (traverse quote (Vector.toList values))
    [| Array Box {values = Vector.fromList $quoted, extra} |]

  Parsed.ArrayUO Box {values, extra} -> do
    let
      quoted =
        fmap ListE (traverse quote (Vector.toList values))
    [| ArrayUO Box {values = Vector.fromList $quoted, extra} |]

  Parsed.Object Box {values, extra} -> do
    let
      quoted =
        fmap (toExp . HashMap.toList) ((traverse . traverse) quote values)
      toExp =
        ListE . map (\(k, v) -> tup2 (LitE (StringL (Text.unpack k)), nonEmptyE v))
      tup2 (a, b) =
        TupE [Just a, Just b]
      nonEmptyE (e :| es) =
        AppE (AppE (ConE '(:|)) e) (ListE es)
    [| Object Box {values = HashMap.fromList $quoted, extra} |]

  Parsed.Sig type_ nullable val ->
    [| Sig type_ nullable $(quote val) |]

  Parsed.Var name ->
    [| Var name |]

  -- | This is fundamentally type-unsafe as long as we try to splice `Exp` in.
  Parsed.Ext exp ->
    [| Ext (toJSONE $(pure exp)) |]

embed :: Aeson.Value -> Matcher
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
  Aeson.Object o ->
    Object Box {values = fmap (pure . embed) (Aeson.toHashMapText o), extra = False}
