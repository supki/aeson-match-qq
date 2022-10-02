{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Aeson.Match.QQ.Internal.Parse
  ( parse
  ) where

import           Control.Applicative ((<|>), optional)
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as ByteString
-- cannot use .Text here due to .Aeson parsers being tied to .ByteString
import           Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
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

import           Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
  , Box(..)
  , HoleSig(..)
  , Type(..)
  )


parse :: ByteString -> Either String (Matcher Exp)
parse =
  Atto.parseOnly value

value :: Atto.Parser (Matcher Exp)
value = do
  spaces
  b <- Atto.peekWord8'
  case b of
    HoleP ->
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
    OpenParenP ->
      arrayUO <|> stringCI
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

any :: Atto.Parser (Matcher Exp)
any = do
  _ <- Atto.word8 HoleP
  name <- fmap Just key <|> pure Nothing
  spaces
  expectedType <- optional holeSig
  pure (Hole expectedType name)

null :: Atto.Parser (Matcher Exp)
null =
  Null <$ Atto.string "null"

false :: Atto.Parser (Matcher Exp)
false =
  Bool False <$ Atto.string "false"

true :: Atto.Parser (Matcher Exp)
true =
  Bool True <$ Atto.string "true"

number :: Atto.Parser (Matcher Exp)
number =
  fmap Number Aeson.scientific

string :: Atto.Parser (Matcher Exp)
string =
  fmap String Aeson.jstring

stringCI :: Atto.Parser (Matcher Exp)
stringCI = do
  _ <- Atto.string "(ci)"
  spaces
  fmap (StringCI . CI.mk) Aeson.jstring

array :: Atto.Parser (Matcher Exp)
array = do
  _ <- Atto.word8 OpenSquareBracketP
  spaces
  b <- Atto.peekWord8'
  case b of
    CloseSquareBracketP -> do
      _ <- Atto.word8 CloseSquareBracketP
      pure (Array Box {values = Vector.empty, extra = False})
    _ -> do
      loop [] 0
 where
  loop acc !n = do
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
              { values = Vector.fromListN (n + 1) (reverse (val : acc))
              , extra = True
              }
          _ ->
            loop (val : acc) (n + 1)
      CloseSquareBracketP ->
        pure $ Array Box
          { values = Vector.fromListN (n + 1) (reverse (val : acc))
          , extra = False
          }
      _ ->
        error "impossible"

arrayUO :: Atto.Parser (Matcher Exp)
arrayUO = do
  _ <- Atto.string "(unordered)"
  spaces
  Array box <- array
  pure (ArrayUO box)

object :: Atto.Parser (Matcher Exp)
object = do
  _ <- Atto.word8 OpenCurlyBracketP
  spaces
  b <- Atto.peekWord8'
  case b of
    CloseCurlyBracketP -> do
      _ <- Atto.word8 CloseCurlyBracketP
      pure (Object Box {values = HashMap.empty, extra = False})
    _ ->
      loop []
 where
  loop acc = do
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
              { values = HashMap.fromList ((k, val) : acc)
              , extra = True
              }
          _ ->
            loop ((k, val) : acc)
      CloseCurlyBracketP ->
        pure $ Object Box
          { values = HashMap.fromList ((k, val) : acc)
          , extra = False
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

haskellExp :: Atto.Parser (Matcher Exp)
haskellExp =
  fmap Ext (Atto.string "#{" *> go)
 where
  go = do
    str <- Atto.takeWhile1 (/= CloseCurlyBracketP) <* Atto.word8 CloseCurlyBracketP
    either fail pure (parseExp (Text.unpack (Text.decodeUtf8 str)))

holeSig :: Atto.Parser HoleSig
holeSig = do
  _ <- Atto.word8 ColonP
  spaces
  asum
    [ p "bool" BoolT
    , p "number" NumberT
    , p "string" StringT
    , p "ci-string" StringCIT
    , p "array" ArrayT
    , p "unordered-array" ArrayUOT
    , p "object" ObjectT
    ]
 where
  p name typeName = do
    _ <- Atto.string name
    q <- optional (Atto.word8 QuestionMarkP)
    pure (HoleSig typeName (isJust q))

-- This function has been stolen from aeson.
-- ref: https://hackage.haskell.org/package/aeson-1.4.6.0/docs/src/Data.Aeson.Parser.Internal.html#skipSpace
spaces :: Atto.Parser ()
spaces =
  Atto.skipWhile (\b -> b == SpaceP || b == NewLineP || b == CRP || b == TabP)
{-# INLINE spaces #-}

pattern HoleP, NP, FP, TP, DoubleQuoteP, DotP, CommaP, HashP :: Word8
pattern HoleP = 95 -- '_'
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

pattern OpenParenP :: Word8
pattern OpenParenP = 40 -- '('
-- pattern CloseParenP :: Word8
-- pattern CloseParenP = 41 -- ')'

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
