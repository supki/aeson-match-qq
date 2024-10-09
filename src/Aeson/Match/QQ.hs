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

import Data.String (IsString(..))
import Data.Text.Encoding qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.PrettyPrint qualified as PP (render)
import Text.PrettyPrint.HughesPJClass qualified as PP (Pretty(..))

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
import Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
  , Box(..)
  , Array
  , Object
  , Type(..)
  , quote
  )


-- | Construct a 'Matcher'.
qq :: QuasiQuoter
qq = QuasiQuoter
  { quoteExp = \str ->
      case parse (Text.encodeUtf8 (fromString str)) of
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

-- | Pretty print an 'Error'.
prettyError :: Error -> String
prettyError =
  PP.render . PP.pPrint
