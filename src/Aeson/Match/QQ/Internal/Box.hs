{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
module Aeson.Match.QQ.Internal.Box
  ( Box(..)
  , Array
  , Object
  ) where

import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Vector (Vector)


-- | A wrapper for matchers that support the `...` syntax.
data Box a = Box
  { values :: a
  , extra  :: Bool
    -- ^ Are extra elements, not specifically mentioned by
    -- the 'Matcher', allowed in the 'Value'?
  } deriving (Show, Eq, Functor)

type Array a = Box (Vector a)

type Object a = Box (HashMap Text (NonEmpty a))
