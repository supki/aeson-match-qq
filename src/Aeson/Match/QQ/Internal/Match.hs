{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Aeson.Match.QQ.Internal.Match where

import           Control.Applicative (liftA2)
import           Control.Monad (unless)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.Either.Validation (Validation, eitherToValidation)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty)
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Prelude hiding (any, null)

import           Aeson.Match.QQ.Internal.Value (Value(..), Box(..))


match :: Value Aeson.Value -> Aeson.Value -> Validation (NonEmpty VE) (HashMap Text Aeson.Value)
match =
  go []
 where
  go path matcher given = do
    let mismatched = mismatch (reverse path) matcher given
    case (matcher, given) of
      (Any Nothing, _) ->
        pure mempty
      (Any (Just name), val) ->
        pure (HashMap.singleton name val)
      (Null, Aeson.Null) ->
        pure mempty
      (Null, _) -> do
        mismatched
        pure mempty
      (Bool b, Aeson.Bool b') -> do
        unless (b == b') mismatched
        pure mempty
      (Bool _, _) -> do
        mismatched
        pure mempty
      (Number n, Aeson.Number n') -> do
        unless (n == n') mismatched
        pure mempty
      (Number _, _) -> do
        mismatched
        pure mempty
      (String str, Aeson.String str') -> do
        unless (str == str') mismatched
        pure mempty
      (String _, _) -> do
        mismatched
        pure mempty
      (Array Box {knownValues, extendable}, Aeson.Array arr) ->
        let fold f =
              Vector.ifoldr (\i v a -> liftA2 HashMap.union a (f i v)) (pure mempty)
            extraValues =
              Vector.drop (Vector.length knownValues) arr
        in
          unless
            (extendable || Vector.null extraValues)
            (extraArrayValues (reverse path) extraValues) *>
          fold
            (\i v -> maybe (missingPathElem (reverse path) (Idx i)) (go (Idx i : path) v) (arr Vector.!? i))
            knownValues
      (Array _, _) -> do
        mismatched
        pure mempty
      (Object Box {knownValues, extendable}, Aeson.Object o) ->
        let fold f =
              HashMap.foldrWithKey (\k v a -> liftA2 HashMap.union a (f k v)) (pure mempty)
            extraValues =
              HashMap.difference o knownValues
        in
          unless
            (extendable || HashMap.null extraValues)
            (extraObjectValues (reverse path) extraValues) *>
          fold
            (\k v -> maybe (missingPathElem (reverse path) (Key k)) (go (Key k : path) v) (HashMap.lookup k o))
            knownValues
      (Object _, _) -> do
        mismatched
        pure mempty
      (Ext val, val') -> do
        unless (val == val') mismatched
        pure mempty

mismatch :: Path -> Value Aeson.Value -> Aeson.Value -> Validation (NonEmpty VE) a
mismatch path matcher given =
  throwE (Mismatch MkMismatch {..})

missingPathElem :: Path -> PathElem -> Validation (NonEmpty VE) a
missingPathElem path missing =
  throwE (MissingPathElem MkMissingPathElem {..})

extraArrayValues :: Path -> Vector Aeson.Value -> Validation (NonEmpty VE) a
extraArrayValues path values =
  throwE (ExtraArrayValues MkExtraArrayValues {..})

extraObjectValues :: Path -> HashMap Text Aeson.Value -> Validation (NonEmpty VE) a
extraObjectValues path values =
  throwE (ExtraObjectValues MkExtraObjectValues {..})

throwE :: e -> Validation (NonEmpty e) a
throwE =
  eitherToValidation . Left . pure

data VE
  = Mismatch Mismatch
  | MissingPathElem MissingPathElem
  | ExtraArrayValues ExtraArrayValues
  | ExtraObjectValues ExtraObjectValues
    deriving (Show, Eq)

instance Aeson.ToJSON VE where
  toJSON =
    Aeson.object . \case
      Mismatch v ->
        [ "type" .= ("mismatch" :: Text)
        , "value" .= v
        ]
      MissingPathElem v ->
        [ "type" .= ("missing-path-elem" :: Text)
        , "value" .= v
        ]
      ExtraArrayValues v ->
        [ "type" .= ("extra-array-values" :: Text)
        , "value" .= v
        ]
      ExtraObjectValues v ->
        [ "type" .= ("extra-object-values" :: Text)
        , "value" .= v
        ]

data MissingPathElem = MkMissingPathElem
  { path :: Path
  , missing :: PathElem
  } deriving (Show, Eq)

instance Aeson.ToJSON MissingPathElem where
  toJSON MkMissingPathElem {..} =
    Aeson.object
      [ "path" .= path
      , "missing" .= missing
      ]

data Mismatch = MkMismatch
  { path :: Path
  , matcher :: Value Aeson.Value
  , given :: Aeson.Value
  } deriving (Show, Eq)

instance Aeson.ToJSON Mismatch where
  toJSON MkMismatch {..} =
    Aeson.object
      [ "path" .= path
      , "matcher" .= matcher
      , "given" .= given
      ]

data ExtraArrayValues = MkExtraArrayValues
  { path :: Path
  , values :: Vector Aeson.Value
  } deriving (Show, Eq)

instance Aeson.ToJSON ExtraArrayValues where
  toJSON MkExtraArrayValues {..} =
    Aeson.object
      [ "path" .= path
      , "values" .= values
      ]

data ExtraObjectValues = MkExtraObjectValues
  { path :: Path
  , values :: HashMap Text Aeson.Value
  } deriving (Show, Eq)

instance Aeson.ToJSON ExtraObjectValues where
  toJSON MkExtraObjectValues {..} =
    Aeson.object
      [ "path" .= path
      , "values" .= values
      ]

type Path = [PathElem]

data PathElem
  = Key Text
  | Idx Int
    deriving (Show, Eq)

instance Aeson.ToJSON PathElem where
  toJSON = \case
    Key k ->
      Aeson.String k
    Idx i ->
      Aeson.Number (fromIntegral i)

instance IsString PathElem where
  fromString =
    Key . fromString
