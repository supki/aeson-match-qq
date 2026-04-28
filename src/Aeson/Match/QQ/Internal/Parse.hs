{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Aeson.Match.QQ.Internal.Parse
  ( Parsed(..)
  , CI(..)
  , StringFrag(..)
  , parse
  ) where

import Control.Applicative ((<|>), optional)
import Data.Aeson.Parser qualified as Aeson
import Data.Attoparsec.ByteString qualified as Atto
import Data.ByteString qualified as ByteString
-- cannot use .Text here due to .Aeson parsers being tied to .ByteString
import Data.ByteString (ByteString)
import Data.Char qualified as Char
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (isJust, fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Data.Word (Word8)
import "ghc-hs-meta" Language.Haskell.Meta.Parse (parseExpWithExts)
import Language.Haskell.TH (Exp(..))
import Language.Haskell.TH.Syntax qualified as Haskell (Extension)
import Prelude hiding (any, exp, null)
import Text.Printf (printf)

import Aeson.Match.QQ.Internal.Box (Box(..), Array, Object)
import Aeson.Match.QQ.Internal.Type (Type(..))


data Parsed
  = Null
  | Bool Bool
  | Number Scientific
  | String CI (NonEmpty StringFrag)
  | Array (Array Parsed)
  | ArrayUO (Array Parsed)
  | Object (Object Parsed)
  | Var Text
  | Sig Type Bool Parsed
  | Ext Exp
    deriving (Show, Eq)

data CI
  = CS -- ^ case-sensitive
  | CI -- ^ case-insensitive
    deriving (Show, Eq)

data StringFrag
  = Raw Text
  | Interpolate Exp
    deriving (Show, Eq)

-- | An 'attoparsec' parser for a 'Matcher'.
--
-- /Note:/ consumes spaces before and after the matcher.
parse :: [Haskell.Extension] -> ByteString -> Either String Parsed
parse exts =
  Atto.parseOnly (value exts <* eof)

value :: [Haskell.Extension] -> Atto.Parser Parsed
value exts = do
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
        string exts
      OpenSquareBracketP ->
        array exts
      OpenParenP ->
        arrayUO exts <|> stringCI exts
      OpenCurlyBracketP ->
        object exts
      HashP ->
        haskellExp exts
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

optimize :: Parsed -> Parsed
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

any :: Atto.Parser Parsed
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

null :: Atto.Parser Parsed
null =
  Null <$ Atto.string "null"

false :: Atto.Parser Parsed
false =
  Bool False <$ Atto.string "false"

true :: Atto.Parser Parsed
true =
  Bool True <$ Atto.string "true"

number :: Atto.Parser Parsed
number =
  fmap Number Aeson.scientific

string :: [Haskell.Extension] -> Atto.Parser Parsed
string exts = do
  str <- Aeson.jstring
  either fail (pure . String CS) (fragments exts str)

stringCI :: [Haskell.Extension] -> Atto.Parser Parsed
stringCI exts = do
  _ <- Atto.string "(ci)"
  spaces
  str <- Aeson.jstring
  either fail (pure . String CI) (fragments exts str)

fragments :: [Haskell.Extension] -> Text -> Either String (NonEmpty StringFrag)
fragments exts str = do
  frags <- Atto.parseOnly (interpolatedP exts <* eof) (Text.encodeUtf8 str)
  pure (fromMaybe (pure (Raw "")) (nonEmpty frags))

interpolatedP :: [Haskell.Extension] -> Atto.Parser [StringFrag]
interpolatedP exts =
  go []
 where
  go acc = do
    b0 <- Atto.peekWord8
    case b0 of
      Just BackSlashP -> do
        _ <- Atto.anyWord8
        b1 <- Atto.peekWord8
        case b1 of
          Just HashP -> do
            _ <- Atto.anyWord8
            go (Raw "#" : acc)
          _ ->
            go (Raw "\\" : acc)
      Just HashP -> do
        _ <- Atto.anyWord8
        b1 <- Atto.peekWord8
        case b1 of
          Just OpenCurlyBracketP -> do
            _ <- Atto.anyWord8
            str <- Atto.takeWhile (/= CloseCurlyBracketP)
            _ <- Atto.word8 CloseCurlyBracketP
            exp <- parseExp exts (Text.decodeUtf8 str)
            go (Interpolate exp : acc)
          _ ->
            go (Raw "#" : acc)
      Just _ -> do
        frag <- fmap (Raw . Text.decodeUtf8) (Atto.takeWhile1 (\b -> b /= HashP && b /= BackSlashP))
        go (frag : acc)
      Nothing ->
        pure (reverse acc)

array :: [Haskell.Extension] -> Atto.Parser Parsed
array exts = do
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
       val <- value exts
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

arrayUO :: [Haskell.Extension] -> Atto.Parser Parsed
arrayUO exts = do
  _ <- Atto.string "(unordered)"
  spaces
  Array box <- array exts
  pure (ArrayUO box)

object :: [Haskell.Extension] -> Atto.Parser Parsed
object exts = do
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
        val <- value exts
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

haskellExp :: [Haskell.Extension] -> Atto.Parser Parsed
haskellExp exts =
  fmap Ext (Atto.string "#{" *> p)
 where
  p = do
    str <- Atto.takeWhile1 (/= CloseCurlyBracketP) <* Atto.word8 CloseCurlyBracketP
    parseExp exts (Text.decodeUtf8 str)

parseExp :: [Haskell.Extension] -> Text -> Atto.Parser Exp
parseExp exts str =
  case parseExpWithExts exts (Text.unpack str) of
    Left (line, col, err) ->
      fail (printf "%d:%d: %s" line col err)
    Right exp ->
      pure exp

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

spaces :: Atto.Parser ()
spaces =
  comment <|> whitespace <|> pure ()
 where
  comment = do
    _ <- Atto.word8 HashP
    b0 <- Atto.peekWord8
    case b0 of
      Just OpenCurlyBracketP ->
        -- it's possible to rewrite this with Atto.lookAhead,
        -- but I like this version better
        fail "not a comment"
      _ -> do
        Atto.skipWhile (/= NewLineP)
        spaces
  whitespace = do
    _ <- skipWhile1 (\b -> b == SpaceP || b == NewLineP || b == CRP || b == TabP)
    spaces

skipWhile1 :: (Word8 -> Bool) -> Atto.Parser ()
skipWhile1 p = do
  _ <- Atto.satisfy p
  Atto.skipWhile p

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

pattern BackSlashP :: Word8
pattern BackSlashP = 92 -- '\'

pattern QuestionMarkP :: Word8
pattern QuestionMarkP = 63 -- '?'
