{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Mushroom(..)
    , OS(..)
    , UserAgent(..)
    , Variant(..)
    , Version(..)

    , userAgentParser
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Maybe

data Variant = ALPHA
             | Alpha
             | Beta
             | DEBUG
             | MASTER
             | Master
             | PERF
             | PROFILE
             | UIAUTOMATION
             | UIAUTOMATIONDEBUG
             | WILDCARD
             | NorthStar
             deriving (Eq, Show, Read)

data Version = Version { major  :: Int
                       , minor  :: Int
                       , bug    :: Int
                       , hotfix :: Maybe Int
                       } deriving (Eq, Show)

data OS = Android ByteString | IOS ByteString deriving (Eq, Show)

data Mushroom = MUSHROOM | SNAPSHOT | OG deriving (Eq, Show, Read)

data UserAgent = UserAgent { version  :: Version
                           , variant  :: Maybe Variant
                           , device   :: ByteString
                           , os       :: OS
                           , mushroom :: Maybe Mushroom
                           } deriving (Eq, Show)

userAgentParser :: Parser UserAgent
userAgentParser = iosParser <|> androidParser

versionParser :: Parser Version
versionParser = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  bug <- decimal
  hotfix <- option Nothing (Just <$> (char '.' *> decimal))
  return (Version major minor bug hotfix)

readParser :: (Read a) => [ByteString] -> Parser a
readParser vs = do
  variant <- choice (map string vs)
  return . read . unpack $ variant

iosVariant :: Parser Variant
iosVariant = readParser
  ["Alpha", "Beta", "DEBUG", "Master", "WILDCARD", "PERF"]

androidVariant :: Parser Variant
androidVariant = readParser
  ["ALPHA", "Beta", "DEBUG", "PROFILE", "MASTER", "UIAUTOMATION",
   "UIAUTOMATIONDEBUG", "PERF", "WILDCARD", "NorthStar"]

mushroomParser :: Parser Mushroom
mushroomParser = "V/" *> readParser ["MUSHROOM", "SNAPSHOT", "OG"]

iosParser :: Parser UserAgent
iosParser = do
  string "Snapchat/"
  version <- versionParser
  skipSpace
  variant <- option Nothing (Just <$> iosVariant)
  skipSpace
  char '('
  device <- takeTill (== ';')
  char ';'
  skipSpace
  string "iOS" <|> string "Ios"
  skipSpace
  os <- takeTill (== ';')
  char ';'
  skipSpace
  string "gzip)"
  return (UserAgent version variant device (IOS os) Nothing)

androidParser :: Parser UserAgent
androidParser = do
  string "Snapchat/"
  version <- versionParser
  skipSpace
  variant <- option Nothing (Just <$> androidVariant)
  skipSpace
  char '('
  device <- takeTill (== ';')
  char ';'
  skipSpace
  string "Android"
  skipSpace
  os <- takeTill (== ';')
  char ';'
  skipSpace
  string "gzip)"
  mushroom <- option Nothing (Just <$> (skipSpace *> mushroomParser))
  return (UserAgent version variant device (Android os) mushroom)
