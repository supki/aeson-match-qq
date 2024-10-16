{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Aeson.Match.QQ.Internal.Match
  ( match
  , Error(..)
  , TypeMismatch(..)
  , Mismatch(..)
  , MissingPathElem(..)
  , ExtraArrayValues(..)
  , ExtraObjectValues(..)
  , Path(..)
  , PathElem(..)
  ) where

import Control.Monad (unless)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson (toHashMapText)
import Data.Bool (bool)
import Data.CaseInsensitive qualified as CI
import Data.Either.Validation
  ( Validation(..)
  , eitherToValidation
  , validationToEither
  )
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Exts (IsList)
import Prelude hiding (any, null)
import Text.PrettyPrint qualified as PP
  ( vcat
  , hsep
  , brackets
  , text
  , char
  , int
  )
import Text.PrettyPrint.HughesPJClass qualified as PP (Pretty(..))

import Aeson.Match.QQ.Internal.AesonUtils qualified as AesonUtils (pp)
import Aeson.Match.QQ.Internal.PrettyPrint qualified as Matcher (pp)
import Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
  , Box(..)
  , Type(..)
  , embed
  )


-- | Test if a 'Matcher' matches a 'Aeson.Value'.
match
  :: Matcher Aeson.Value
  -> Aeson.Value
  -> Either (NonEmpty Error) (HashMap Text Aeson.Value)
     -- ^ Either a non-empty list of errors, or a mapping
     -- from named _holes to their values.
match matcher0 given0 =
  validationToEither (go [] matcher0 given0)
 where
  go path matcher given = do
    let
      mismatched =
        mismatch path matcher given
      mistyped expected =
        mistype path expected matcher given
    case (matcher, given) of
      (Null, Aeson.Null) ->
        pure mempty
      (Null, _) -> do
        mismatched
        pure mempty
      (Bool b, Aeson.Bool b') -> do
        unless (b == b') mismatched
        pure mempty
      (Bool _, _) -> do
        mistyped BoolT
        pure mempty
      (Number n, Aeson.Number n') -> do
        unless (n == n') mismatched
        pure mempty
      (Number _, _) -> do
        mistyped NumberT
        pure mempty
      (String str, Aeson.String str') -> do
        unless (str == str') mismatched
        pure mempty
      (String _, _) -> do
        mistyped StringT
        pure mempty
      (StringCI str, Aeson.String str') -> do
        unless (str == CI.mk str') mismatched
        pure mempty
      (StringCI _, _) -> do
        mistyped StringCIT
        pure mempty
      (Array Box {values, extra}, Aeson.Array arr) ->
        let
          fold f =
            Vector.ifoldr (\i v a -> liftA2 HashMap.union a (f i v)) (pure mempty)
          extraValues =
            Vector.drop (Vector.length values) arr
        in
          unless
            (extra || Vector.null extraValues)
            (extraArrayValues path extraValues) *>
          fold
            (\i v -> maybe (missingPathElem path (Idx i)) (go (Idx i : path) v) (arr Vector.!? i))
            values
      (Array _, _) -> do
        mistyped ArrayT
        pure mempty
      (ArrayUO box, Aeson.Array arr) ->
        matchArrayUO mismatched path box arr
      (ArrayUO _, _) -> do
        mistyped ArrayUOT
        pure mempty
      ( Object Box {values, extra}
        , Aeson.Object (Aeson.toHashMapText -> o)

        ) ->
        let fold f =
              HashMap.foldrWithKey (\k v a -> liftA2 HashMap.union a (f k v)) (pure mempty)
            extraValues =
              HashMap.difference o values
        in
          unless
            (extra || HashMap.null extraValues)
            (extraObjectValues path extraValues) *>
          fold
            (\k vs ->
              maybe
                (missingPathElem path (Key k))
                (\ov -> foldr1 (liftA2 (<>)) (fmap (\v -> go (Key k : path) v ov) vs))
                (HashMap.lookup k o))
            values
      (Object _, _) -> do
        mistyped ObjectT
        pure mempty
      (Sig type_ nullable x, val) -> -- do -- ApplicativeDo shits the bed here for some reason
        unless (sigTypeMatch type_ nullable val) (mistyped type_) *>
        go path x val
      (Var "", _) ->
        pure mempty
      (Var name, val) ->
        pure (HashMap.singleton name val)
      (Ext val, val') ->
        go path (embed val) val'

sigTypeMatch :: Type -> Bool -> Aeson.Value -> Bool
sigTypeMatch type_ nullable val =
  case (type_, nullable, val) of
    (AnyT,      _,    _) -> True
    (_,         True, Aeson.Null) -> True
    (BoolT,     _,    Aeson.Bool {}) -> True
    (NumberT,   _,    Aeson.Number {}) -> True
    (StringT,   _,    Aeson.String {}) -> True
    (StringCIT, _,    Aeson.String {}) -> True
    (ArrayT,    _,    Aeson.Array {}) -> True
    (ArrayUOT,  _,    Aeson.Array {}) -> True
    (ObjectT,   _,    Aeson.Object {}) -> True
    (_,         _,    _) -> False

matchArrayUO
  :: Validation (NonEmpty Error) (HashMap Text Aeson.Value)
  -> [PathElem]
  -> Box (Vector (Matcher Aeson.Value))
  -> Vector Aeson.Value
  -> Validation (NonEmpty Error) (HashMap Text Aeson.Value)
matchArrayUO mismatched path Box {values, extra} xs = do
  -- Collect possible indices in `xs` for each position in `values`.
  let indices = map (collectMatchingIndices (toList xs)) (toList values)
  -- Find all unique valid ways to map each position in `values` to
  -- a member of `xs`.
  case allIndicesAssignments indices of
    -- If no assignment has been found, we give up.
    [] ->
      mismatched
    ivs : _
      -- If some positions in `values` cannot be mapped to
      -- anything in `xs`, we give up.
      | length ivs < length values ->
        mismatched
      -- If there are some members of `xs` that aren't matched by
      -- anything in `values`, we check if the 'Matcher' allows for
      -- extra values.
      | length ivs < length xs && not extra -> do
        let is = Set.fromList (map fst ivs)
            extraValues = Vector.ifilter (\i _ -> not (i `Set.member` is)) xs
        extraArrayValues path extraValues
      | otherwise ->
        pure (foldMap snd ivs)
 where
  collectMatchingIndices is knownValue =
    imapMaybe matchingIndex is
   where
    matchingIndex i x =
      case match knownValue x of
        Left _ ->
          Nothing
        Right vs ->
          Just (i, vs)
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

mismatch
  :: [PathElem]
  -> Matcher Aeson.Value
  -> Aeson.Value
  -> Validation (NonEmpty Error) a
mismatch (Path . reverse -> path) matcher given =
  throwE (Mismatch MkMismatch {..})

mistype
  :: [PathElem]
  -> Type
  -> Matcher Aeson.Value
  -> Aeson.Value
  -> Validation (NonEmpty Error) a
mistype (Path . reverse -> path) expected matcher given =
  throwE (Mistype MkTypeMismatch {..})

missingPathElem
  :: [PathElem]
  -> PathElem
  -> Validation (NonEmpty Error) a
missingPathElem (Path . reverse -> path) missing =
  throwE (MissingPathElem MkMissingPathElem {..})

extraArrayValues
  :: [PathElem]
  -> Vector Aeson.Value
  -> Validation (NonEmpty Error) a
extraArrayValues (Path . reverse -> path) values =
  throwE (ExtraArrayValues MkExtraArrayValues {..})

extraObjectValues
  :: [PathElem]
  -> HashMap Text Aeson.Value
  -> Validation (NonEmpty Error) a
extraObjectValues (Path . reverse -> path) values =
  throwE (ExtraObjectValues MkExtraObjectValues {..})

throwE :: e -> Validation (NonEmpty e) a
throwE =
  eitherToValidation . Left . pure

-- | Various errors that can happen when a matcher tries to match a 'Aeson.Value'.
data Error
  = Mismatch Mismatch
    -- ^ The type of the value is correct, but the value itself is wrong
  | Mistype TypeMismatch
    -- ^ The type of the value is wrong
  | MissingPathElem MissingPathElem
    -- ^ The request path is missing in the value
  | ExtraArrayValues ExtraArrayValues
    -- ^ Unexpected extra values in an array
  | ExtraObjectValues ExtraObjectValues
    -- ^ Unexpected extra key-value pairs in an object
    deriving (Show, Eq)

instance PP.Pretty Error where
  pPrint = \case
    Mismatch err ->
      PP.vcat
        [ "  error: value does not match"
        , PP.pPrint err
        ]
    Mistype err ->
      PP.vcat
        [ "   error: type of value does not match"
        , PP.pPrint err
        ]
    MissingPathElem err ->
      PP.vcat
        [ "  error: missing key or index"
        , PP.pPrint err
        ]
    ExtraArrayValues err ->
      PP.vcat
        [ "  error: extra array values"
        , PP.pPrint err
        ]
    ExtraObjectValues err ->
      PP.vcat
        [ "  error: extra object values"
        , PP.pPrint err
        ]

instance Aeson.ToJSON Error where
  toJSON =
    Aeson.object . \case
      Mismatch v ->
        [ "type" .= ("mismatch" :: Text)
        , "value" .= v
        ]
      Mistype v ->
        [ "type" .= ("mistype" :: Text)
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

-- | This error type covers the case where the type of the value does not match.
data TypeMismatch = MkTypeMismatch
  { path     :: Path
  , expected :: Type
  , matcher  :: Matcher Aeson.Value
  , given    :: Aeson.Value
  } deriving (Show, Eq)

instance Aeson.ToJSON TypeMismatch where
  toJSON MkTypeMismatch {..} =
    Aeson.object
      [ "path" .= path
      , "expected" .= expected
      , "actual" .= typeJOf given
      , "matcher" .= matcher
      , "given" .= given
      ]

instance PP.Pretty TypeMismatch where
  pPrint MkTypeMismatch {..} =
    PP.vcat
      [ PP.hsep ["expected:", PP.pPrint expected]
      , PP.hsep ["  actual:", PP.pPrint (typeJOf given)]
      , PP.hsep ["    path:", PP.pPrint path]
      , PP.hsep [" matcher:", Matcher.pp matcher]
      , PP.hsep ["   given:", AesonUtils.pp given]
      ]

-- | JSON value type.
data TypeJ
  = NullTJ
  | BoolTJ
  | NumberTJ
  | StringTJ
  | ArrayTJ
  | ObjectTJ
    deriving (Show, Eq)

instance Aeson.ToJSON TypeJ where
  toJSON =
    Aeson.toJSON . \case
      NullTJ -> "null" :: Text
      BoolTJ -> "bool"
      NumberTJ -> "number"
      StringTJ -> "string"
      ArrayTJ -> "array"
      ObjectTJ -> "object"

instance PP.Pretty TypeJ where
  pPrint = \case
    NullTJ {} -> "null"
    BoolTJ {} -> "bool"
    NumberTJ {} -> "number"
    StringTJ {} -> "string"
    ArrayTJ {} -> "array"
    ObjectTJ {} -> "object"

typeJOf :: Aeson.Value -> TypeJ
typeJOf = \case
  Aeson.Null -> NullTJ
  Aeson.Bool {} -> BoolTJ
  Aeson.Number {} -> NumberTJ
  Aeson.String {} -> StringTJ
  Aeson.Array {} -> ArrayTJ
  Aeson.Object {} -> ObjectTJ

-- | This error type covers the case where the type matches but the value does not.
data Mismatch = MkMismatch
  { path    :: Path
  , matcher :: Matcher Aeson.Value
  , given   :: Aeson.Value
  } deriving (Show, Eq)

instance Aeson.ToJSON Mismatch where
  toJSON MkMismatch {..} =
    Aeson.object
      [ "path" .= path
      , "matcher" .= matcher
      , "given" .= given
      ]

instance PP.Pretty Mismatch where
  pPrint MkMismatch {..} =
    PP.vcat
      [ PP.hsep ["   path:", PP.pPrint path]
      , PP.hsep ["matcher:", Matcher.pp matcher]
      , PP.hsep ["  given:", AesonUtils.pp given]
      ]

-- | This error type covers the case where the requested path simply does not exist
-- in a 'Aeson.Value'.
data MissingPathElem = MkMissingPathElem
  { path    :: Path
  , missing :: PathElem
  } deriving (Show, Eq)

instance Aeson.ToJSON MissingPathElem where
  toJSON MkMissingPathElem {..} =
    Aeson.object
      [ "path" .= path
      , "missing" .= missing
      ]

instance PP.Pretty MissingPathElem where
  pPrint (MkMissingPathElem {..}) =
    PP.vcat
      [ PP.hsep ["   path:", PP.pPrint path]
      , PP.hsep ["missing:", PP.pPrint missing]
      ]

-- | Unless an permissive matcher is used, any extra values in an array
-- missing in the matcher will trigger this error.
data ExtraArrayValues = MkExtraArrayValues
  { path   :: Path
  , values :: Vector Aeson.Value
  } deriving (Show, Eq)

instance Aeson.ToJSON ExtraArrayValues where
  toJSON MkExtraArrayValues {..} =
    Aeson.object
      [ "path" .= path
      , "values" .= values
      ]

instance PP.Pretty ExtraArrayValues where
  pPrint MkExtraArrayValues {..} =
    PP.vcat
      [ PP.hsep ["   path:", PP.pPrint path]
      , PP.hsep
          [ " values:"
          , PP.vcat (map AesonUtils.pp (toList values))
          ]
      ]

-- | Unless an permissive matcher is used, any extra key-value pairs in
-- an object missing in the matcher will trigger this error.
data ExtraObjectValues = MkExtraObjectValues
  { path   :: Path
  , values :: HashMap Text Aeson.Value
  } deriving (Show, Eq)

instance Aeson.ToJSON ExtraObjectValues where
  toJSON MkExtraObjectValues {..} =
    Aeson.object
      [ "path" .= path
      , "values" .= values
      ]

instance PP.Pretty ExtraObjectValues where
  pPrint MkExtraObjectValues {..} =
    PP.vcat
      [ PP.hsep ["   path:", PP.pPrint path]
      , PP.hsep
          [ " values:"
          , PP.vcat ((map prettyKV . List.sortOn fst . HashMap.toList) values)
          ]
      ]
   where
    prettyKV (k, v) =
      PP.vcat
        [ PP.hsep ["  key:", PP.pPrint (Key k)]
        , PP.hsep ["value:", AesonUtils.pp v]
        ]

-- | A path is a list of path elements.
newtype Path = Path { unPath :: [PathElem] }
    deriving (Show, Eq, IsList, Aeson.ToJSON)

instance PP.Pretty Path where
  pPrint =
    maybe "." (foldMap PP.pPrint) . nonEmpty . unPath

-- | A path element is either a key lookup in an object, or an index lookup in an array.
data PathElem
  = Key Text
  | Idx Int
    deriving (Show, Eq)

instance IsString PathElem where
  fromString =
    Key . fromString

instance Aeson.ToJSON PathElem where
  toJSON = \case
    Key k ->
      Aeson.String k
    Idx i ->
      Aeson.Number (fromIntegral i)

instance PP.Pretty PathElem where
  pPrint = \case
    Key k ->
      PP.char '.' <> PP.text (Text.unpack k)
    Idx i ->
      PP.brackets (PP.int i)

imapMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
imapMaybe f =
  mapMaybe (uncurry f) . zip [0..]
