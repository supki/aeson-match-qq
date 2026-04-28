{-# LANGUAGE DuplicateRecordFields #-}
module Aeson.Match.QQ
  ( match
  , qq

  , Error(..)
  , TypeMismatch(..)
  , Mismatch(..)
  , MissingPathElem(..)
  , ExtraArrayValues(..)
  , ExtraObjectValues(..)
  , prettyError

  , Matcher(..)
  , Array
  , Object
  , Box(..)
  , Type(..)
  , Path(..)
  , PathElem(..)

  , parse
  ) where

import Control.Monad (filterM)
import Data.String (IsString(..))
import Data.Text.Encoding qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (isExtEnabled)
import Text.PrettyPrint qualified as PP (render)
import Text.PrettyPrint.HughesPJClass qualified as PP (Pretty(..))

import Aeson.Match.QQ.Internal.Box (Box(..), Array, Object)
import Aeson.Match.QQ.Internal.Match
  ( match
  , Error(..)
  , TypeMismatch(..)
  , Mismatch(..)
  , MissingPathElem(..)
  , ExtraArrayValues(..)
  , ExtraObjectValues(..)
  , Path(..)
  , PathElem(..)
  )
import Aeson.Match.QQ.Internal.Parse (parse)
import Aeson.Match.QQ.Internal.Value (Matcher(..), quote)
import Aeson.Match.QQ.Internal.Type (Type(..))


-- | Construct a 'Matcher'.
qq :: QuasiQuoter
qq = QuasiQuoter
  { quoteExp = \str -> do
      exts <- reifyEnabledExtensions
      case parse exts (Text.encodeUtf8 (fromString str)) of
        Left err ->
          error ("Aeson.Match.QQ.qq: " ++ err)
        Right val ->
          quote val
  , quotePat =
      \_ -> error "Aeson.Match.QQ.qq: no quotePat"
  , quoteType =
      \_ -> error "Aeson.Match.QQ.qq: no quoteType"
  , quoteDec =
      \_ -> error "Aeson.Match.QQ.qq: no quoteDec"
  }
 where
  reifyEnabledExtensions =
    filterM isExtEnabled [minBound .. maxBound]

-- | Pretty print an 'Error'.
prettyError :: Error -> String
prettyError =
  PP.render . PP.pPrint
