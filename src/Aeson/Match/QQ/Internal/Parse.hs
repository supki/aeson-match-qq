{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Aeson.Match.QQ.Internal.Parse
  ( parse
  ) where

import Control.Applicative ((<|>), optional)
import Data.Aeson.Parser qualified as Aeson
import Data.Attoparsec.ByteString qualified as Atto
import Data.ByteString qualified as ByteString
-- cannot use .Text here due to .Aeson parsers being tied to .ByteString
import Data.ByteString (ByteString)
import Data.CaseInsensitive qualified as CI
import Data.Char qualified as Char
import Data.Foldable (asum)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Data.Word (Word8)
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH (Exp(..))
import Prelude hiding (any, null)

import Aeson.Match.QQ.Internal.Value
  ( Matcher(..)
  , Box(..)
  , Type(..)
  )


-- | An 'attoparsec' parser for a 'Matcher'.
--
-- /Note:/ consumes spaces before and after the matcher.
parse :: ByteString -> Either String (Matcher Exp)
parse =
  Atto.parseOnly (value <* eof)

value :: Atto.Parser (Matcher Exp)
value = do
  val <- between spaces spaces $ do
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
  pure (optimize val)
 where
  startOfNumber b =
    b >= ZeroP && b <= NineP || b == MinusP
  between a b p =
    a *> p <* b

optimize :: Matcher Exp -> Matcher Exp
optimize = \case
  -- [...] -> _ : array
  Array Box {extra = True, values = (Vector.null -> True)} ->
    Sig ArrayT False (Var "")
  -- this optimization is probably never going to be used,
  -- but I'll include it for completeness:
  -- (unordered) [...] -> _ : unordered-array
  ArrayUO Box {extra = True, values = (Vector.null -> True)} ->
    Sig ArrayUOT False (Var "")
  -- {...} -> _ : object
  Object Box {extra = True, values = (HashMap.null -> True)} ->
    Sig ObjectT False (Var "")
  val ->
    val

any :: Atto.Parser (Matcher Exp)
any = do
  _ <- Atto.word8 HoleP
  name <- key <|> pure ""
  spaces
  b <- optional Atto.peekWord8'
  (type_, nullable) <- case b of
    Just ColonP ->
      sig
    _ ->
      pure (AnyT, False)
  pure (Sig type_ nullable (Var name))

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
    spaces
    b <- Atto.peekWord8'
    case b of
      DotP -> do
       rest
       spaces
       _ <- Atto.word8 CloseSquareBracketP
       pure $ Array Box
         { values = Vector.fromListN (n + 1) (reverse acc)
         , extra = True
         }
      _ -> do
       val <- value
       sep <- Atto.satisfy (\w -> w == CommaP || w == CloseSquareBracketP) Atto.<?> "',' or ']'"
       case sep of
         CommaP ->
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
    spaces
    b <- Atto.peekWord8'
    case b of
      DotP -> do
        rest
        spaces
        _ <- Atto.word8 CloseCurlyBracketP
        pure $ Object Box
          { values = HashMap.fromList acc
          , extra = True
          }
      _ -> do
        k <- key
        spaces
        _ <- Atto.word8 ColonP
        val <- value
        sep <- Atto.satisfy (\w -> w == CommaP || w == CloseCurlyBracketP) Atto.<?> "',' or '}'"
        case sep of
          CommaP ->
            loop ((k, pure val) : acc)
          CloseCurlyBracketP ->
            pure $ Object Box
              { values = HashMap.fromListWith (<>) ((k, pure val) : acc)
              , extra = False
              }
          _ ->
            error "impossible"

key :: Atto.Parser Text
key =
  Aeson.jstring <|> bareKey

bareKey :: Atto.Parser Text
bareKey =
  fmap
    (Text.decodeUtf8 . ByteString.pack)
    (Atto.many1
      (Atto.satisfy
        (p . Char.chr . fromIntegral)))
 where
  p c =
    not (Char.isSpace c || c `elem` ("\\\":;><${}[],#" :: String))

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

sig :: Atto.Parser (Type, Bool)
sig = do
  _ <- Atto.word8 ColonP
  spaces
  asum
    [ p "any" AnyT
    , p "bool" BoolT
    , p "number" NumberT
    , p "string" StringT
    , p "ci-string" StringCIT
    , p "array" ArrayT
    , p "unordered-array" ArrayUOT
    , p "object" ObjectT
    ] Atto.<?> "unknown type in hole signature"
 where
  p name typeName = do
    _ <- Atto.string name
    q <- optional (Atto.word8 QuestionMarkP)
    pure (typeName, isJust q)

eof :: Atto.Parser ()
eof =
  Atto.endOfInput Atto.<?> "trailing garbage after a Matcher value"

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
