module Aeson.Match.QQ
  ( Value(..)
  , Array
  , Object
  , Box(..)
  , TypeSig(..)
  , Type(..)
  , Nullable(..)
  , Path
  , PathElem(..)
  , parse
  , qq
  , match
  , mismatch
  , missingPathElem
  , extraArrayValues
  , extraObjectValues
  ) where

import           Data.String (IsString(..))
import qualified Data.Text.Encoding as Text
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.TH.Syntax (Lift(..))

import           Aeson.Match.QQ.Internal.Match (Path, PathElem(..), match, mismatch, missingPathElem, extraArrayValues, extraObjectValues)
import           Aeson.Match.QQ.Internal.Parse (parse)
import           Aeson.Match.QQ.Internal.Value (Value(..), Box(..), Array, Object, TypeSig(..), Type(..), Nullable(..))


qq :: QuasiQuoter
qq = QuasiQuoter
  { quoteExp = \str ->
      case parse (Text.encodeUtf8 (fromString str)) of
        Left err ->
          error ("Aeson.Match.QQ.qq: " ++ err)
        Right val ->
          lift val
  , quotePat =
      \_ -> error "Aeson.Match.QQ.qq: no quotePat"
  , quoteType =
      \_ -> error "Aeson.Match.QQ.qq: no quoteType"
  , quoteDec =
      \_ -> error "Aeson.Match.QQ.qq: no quoteDec"
  }
