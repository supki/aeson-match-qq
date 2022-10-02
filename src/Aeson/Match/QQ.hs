module Aeson.Match.QQ
  ( match
  , qq

  , Error(..)
  , Mismatch(..)
  , MissingPathElem(..)
  , ExtraArrayValues(..)
  , ExtraObjectValues(..)
  , prettyError

  , Matcher(..)
  , Array
  , Object
  , Box(..)
  , HoleSig(..)
  , Type(..)
  , Path(..)
  , PathElem(..)
  ) where

import           Data.String (IsString(..))
import qualified Data.Text.Encoding as Text
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.TH.Syntax (Lift(..))
import qualified Text.PrettyPrint as PP (render)
import qualified Text.PrettyPrint.HughesPJClass as PP (Pretty(..))

import           Aeson.Match.QQ.Internal.Match
  ( match
  , Error(..)
  , Mismatch(..)
  , MissingPathElem(..)
  , ExtraArrayValues(..)
  , ExtraObjectValues(..)
  , Path(..)
  , PathElem(..)
  )
import           Aeson.Match.QQ.Internal.Parse (parse)
import           Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
  , Box(..)
  , Array
  , Object
  , HoleSig(..)
  , Type(..)
  )


-- | Construct a 'Matcher'.
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

-- | Pretty print an 'Error'.
prettyError :: Error -> String
prettyError =
  PP.render . PP.pPrint
