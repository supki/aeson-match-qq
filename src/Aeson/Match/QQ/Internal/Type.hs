{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Aeson.Match.QQ.Internal.Type
  ( Type(..)
  ) where

import Language.Haskell.TH.Syntax (Lift(..))
import Text.PrettyPrint.HughesPJClass qualified as PP (Pretty(..))


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
