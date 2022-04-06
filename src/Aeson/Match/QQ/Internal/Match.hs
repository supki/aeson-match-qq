{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Aeson.Match.QQ.Internal.Match where

import           Control.Applicative (liftA2)
import           Control.Monad (unless)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson (toHashMapText)
import           Data.Either.Validation (Validation(..), eitherToValidation)
import           Data.Foldable (for_, toList)
import           Data.Bool (bool)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Prelude hiding (any, null)

import           Aeson.Match.QQ.Internal.Value (Value(..), Box(..), TypeSig(..), Type(..), Nullable(..))


match
  :: Value Aeson.Value
  -> Aeson.Value
  -> Validation (NonEmpty VE) (HashMap Text Aeson.Value)
match =
  go []
 where
  go path matcher given = do
    let mismatched = mismatch (reverse path) matcher given
    case (matcher, given) of
      (Any holeTypeO nameO, val) -> do
        for_ holeTypeO $ \holeType ->
          unless (holeTypeMatch holeType val)
            mismatched
        pure (maybe mempty (\name -> HashMap.singleton name val) nameO)
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
        let
          fold f =
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
      (ArrayUO box, Aeson.Array arr) ->
        matchArrayUO mismatched path box arr
      (ArrayUO _, _) -> do
        mismatched
        pure mempty
      (Object Box {knownValues, extendable}, Aeson.Object (Aeson.toHashMapText -> o)) ->
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

holeTypeMatch :: TypeSig -> Aeson.Value -> Bool
holeTypeMatch type_ val =
  case (type_, val) of
    (TypeSig {nullable = Nullable}, Aeson.Null) -> True
    (TypeSig {type_ = BoolT} , Aeson.Bool {}) -> True
    (TypeSig {type_ = NumberT} , Aeson.Number {}) -> True
    (TypeSig {type_ = StringT} , Aeson.String {}) -> True
    (TypeSig {type_ = ArrayT} , Aeson.Array {}) -> True
    (TypeSig {type_ = ObjectT} , Aeson.Object {}) -> True
    (_, _) -> False

matchArrayUO
  :: Validation (NonEmpty VE) (HashMap Text Aeson.Value)
  -> Path
  -> Box (Vector (Value Aeson.Value))
  -> Vector Aeson.Value
  -> Validation (NonEmpty VE) (HashMap Text Aeson.Value)
matchArrayUO mismatched path Box {knownValues, extendable} xs = do
  -- Collect possible indices in `xs` for each position in `knownValues`.
  let indices = map (collectMatchingIndices (toList xs)) (toList knownValues)
  -- Find all unique valid ways to map each position in `knownValues` to
  -- a member of `xs`.
  case allIndicesAssignments indices of
    -- If no assignment has been found, we give up.
    [] ->
      mismatched
    ivs : _
      -- If some positions in `knownValues` cannot be mapped to
      -- anything in `xs`, we give up.
      | length ivs < length knownValues ->
        mismatched
      -- If there are some members of `xs` that aren't matched by
      -- anything in `knownValues`, we check if the pattern is
      -- extendable.
      | length ivs < length xs && not extendable -> do
        let is = Set.fromList (map fst ivs)
            extraValues = Vector.ifilter (\i _ -> not (i `Set.member` is)) xs
        extraArrayValues (reverse path) extraValues
      | otherwise ->
        pure (foldMap snd ivs)
 where
  collectMatchingIndices is knownValue =
    imapMaybe matchingIndex is
   where
    matchingIndex i x =
      case match knownValue x of
        Success vs ->
          Just (i, vs)
        Failure _ ->
          Nothing
  allIndicesAssignments = map (map unI) . cleanUp . go Set.empty
   where
    go _ [] = [[]]
    go known (is : iss) = do
      (i, vs) <- is
      bool (map (I (i, vs) :) (go (Set.insert i known) iss)) [] (i `Set.member` known)
    cleanUp =
      toList . Set.fromList . map (Set.toAscList . Set.fromList)

newtype I = I { unI :: (Int, HashMap Text Aeson.Value) }

instance Eq I where
  I (a, _) == I (b, _) =
    a == b

instance Ord I where
  I (a, _) `compare` I (b, _) =
    a `compare` b

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

imapMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
imapMaybe f =
  mapMaybe (uncurry f) . zip [0..]
