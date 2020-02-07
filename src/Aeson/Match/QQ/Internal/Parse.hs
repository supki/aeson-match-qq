{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Aeson.Match.QQ.Internal.Parse
  ( parse
  ) where

import           Control.Applicative ((<|>), optional)
import qualified Data.Aeson.Parser as Aeson
import qualified Data.ByteString as ByteString
-- cannot use .Text here due to .Aeson parsers being tied to .ByteString
import qualified Data.Attoparsec.ByteString as Atto
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import qualified Data.Char as Char
import           Data.Foldable (asum)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import           Data.Word (Word8)
import           Language.Haskell.Meta.Parse (parseExp)
import           Language.Haskell.TH (Exp(..))
import           Prelude hiding (any, null)

import           Aeson.Match.QQ.Internal.Value (Value(..), Box(..), TypeSig(..), Type(..), Nullable(..))


parse :: ByteString -> Either String (Value Exp)
parse =
  Atto.parseOnly value

value :: Atto.Parser (Value Exp)
value = do
  spaces
  b <- Atto.peekWord8'
  case b of
    AnyP ->
      any
    NP ->
      null
    FP ->
      false
    TP ->
      true
    DoubleQuoteP ->
      string
    OpenSquareBracketP ->
      array
    OpenCurlyBracketP ->
      object
    HashP ->
      haskellExp
    _ | startOfNumber b ->
        number
      | otherwise ->
        fail ("a value cannot start with " ++ show b)
 where
  startOfNumber b =
    b >= ZeroP && b <= NineP || b == MinusP

any :: Atto.Parser (Value Exp)
any = do
  _ <- Atto.word8 AnyP
  name <- fmap Just key <|> pure Nothing
  spaces
  expectedType <- optional typeSig
  pure (Any expectedType name)

null :: Atto.Parser (Value Exp)
null =
  Null <$ Atto.string "null"

false :: Atto.Parser (Value Exp)
false =
  Bool False <$ Atto.string "false"

true :: Atto.Parser (Value Exp)
true =
  Bool True <$ Atto.string "true"

number :: Atto.Parser (Value Exp)
number =
  fmap Number Aeson.scientific

string :: Atto.Parser (Value Exp)
string =
  fmap String Aeson.jstring

array :: Atto.Parser (Value Exp)
array = do
  _ <- Atto.word8 OpenSquareBracketP
  spaces
  b <- Atto.peekWord8'
  case b of
    CloseSquareBracketP ->
      pure (Array Box {knownValues = Vector.empty, extendable = False})
    _ -> do
      loop [] 0
 where
  loop values !n = do
    val <- value
    spaces
    b <- Atto.satisfy (\w -> w == CommaP || w == CloseSquareBracketP) Atto.<?> "',' or ']'"
    case b of
      CommaP -> do
        spaces
        b' <- Atto.peekWord8'
        case b' of
          DotP -> do
            rest
            spaces
            _ <- Atto.word8 CloseSquareBracketP
            pure $ Array Box
              { knownValues = Vector.fromListN (n + 1) (reverse (val : values))
              , extendable = True
              }
          _ ->
            loop (val : values) (n + 1)
      CloseSquareBracketP ->
        pure $ Array Box
          { knownValues = Vector.fromListN (n + 1) (reverse (val : values))
          , extendable = False
          }
      _ ->
        error "impossible"

object :: Atto.Parser (Value Exp)
object = do
  _ <- Atto.word8 OpenCurlyBracketP
  spaces
  b <- Atto.peekWord8'
  case b of
    CloseCurlyBracketP ->
      pure (Object Box {knownValues = HashMap.empty, extendable = False})
    _ ->
      loop []
 where
  loop values = do
    k <- key
    spaces
    _ <- Atto.word8 ColonP
    spaces
    val <- value
    spaces
    b <- Atto.satisfy (\b -> b == CommaP || b == CloseCurlyBracketP) Atto.<?> "',' or '}'"
    case b of
      CommaP -> do
        spaces
        b' <- Atto.peekWord8'
        case b' of
          DotP -> do
            rest
            spaces
            _ <- Atto.word8 CloseCurlyBracketP
            pure $ Object Box
              { knownValues = HashMap.fromList ((k, val) : values)
              , extendable = True
              }
          _ ->
            loop ((k, val) : values)
      CloseCurlyBracketP ->
        pure $ Object Box
          { knownValues = HashMap.fromList ((k, val) : values)
          , extendable = False
          }
      _ ->
        error "impossible"

key :: Atto.Parser Text
key =
  Aeson.jstring <|>
    fmap (Text.decodeUtf8 . ByteString.pack) (Atto.many1 (Atto.satisfy (\c -> Char.chr (fromIntegral c) `notElem` ("\\ \":;><${}[]," :: String))))

rest :: Atto.Parser ()
rest =
  () <$ Atto.string "..."

haskellExp :: Atto.Parser (Value Exp)
haskellExp =
  fmap Ext (Atto.string "#{" *> go)
 where
  go = do
    str <- Atto.takeWhile1 (/= CloseCurlyBracketP) <* Atto.word8 CloseCurlyBracketP
    either fail pure (parseExp (Text.unpack (Text.decodeUtf8 str)))

typeSig :: Atto.Parser TypeSig
typeSig = do
  _ <- Atto.word8 ColonP
  spaces
  asum
    [ p "bool" BoolT
    , p "number" NumberT
    , p "string" StringT
    , p "array" ArrayT
    , p "object" ObjectT
    ]
 where
  p name typeName = do
    _ <- Atto.string name
    q <- optional (Atto.word8 QuestionMarkP)
    pure (TypeSig typeName (bool NonNullable Nullable (isJust q)))

-- This function has been stolen from aeson.
-- ref: https://hackage.haskell.org/package/aeson-1.4.6.0/docs/src/Data.Aeson.Parser.Internal.html#skipSpace
spaces :: Atto.Parser ()
spaces =
  Atto.skipWhile (\b -> b == SpaceP || b == NewLineP || b == CRP || b == TabP)
{-# INLINE spaces #-}

pattern AnyP, NP, FP, TP, DoubleQuoteP, DotP, CommaP, HashP :: Word8
pattern AnyP = 95 -- '_'
pattern NP = 110 -- 'n'
pattern FP = 102 -- 'f'
pattern TP = 116 -- 't'
pattern DoubleQuoteP = 34 -- '"'
pattern CommaP = 44 -- ','
pattern DotP = 46 -- '.'
pattern HashP = 35 -- '#'

pattern OpenSquareBracketP, CloseSquareBracketP :: Word8
pattern OpenSquareBracketP = 91 -- '['
pattern CloseSquareBracketP = 93 -- ']'

pattern OpenCurlyBracketP, CloseCurlyBracketP, ColonP :: Word8
pattern OpenCurlyBracketP = 123 -- '{'
pattern CloseCurlyBracketP = 125 -- '}'
pattern ColonP = 58 -- ':'

pattern ZeroP, NineP, MinusP :: Word8
pattern ZeroP = 48 -- '0'
pattern NineP = 57 -- '9'
pattern MinusP = 45 -- '-'

pattern SpaceP, NewLineP, CRP, TabP :: Word8
pattern SpaceP = 0x20
pattern NewLineP = 0x0a
pattern CRP = 0x0d
pattern TabP = 0x09

pattern QuestionMarkP :: Word8
pattern QuestionMarkP = 63 -- '?'
