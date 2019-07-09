{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Flavor(..)
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

data Flavor = ALPHA
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

data Variant = MUSHROOM | SNAPSHOT | OG deriving (Eq, Show, Read)

data GAESuffix = AppEngine { projectID :: ByteString } deriving (Eq, Show)

data UserAgent = UserAgent { version :: Version
                           , flavor  :: Maybe Flavor
                           , device  :: ByteString
                           , os      :: OS
                           , variant :: Maybe Variant
                           , suffix  :: Maybe GAESuffix
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

iosFlavor :: Parser Flavor
iosFlavor = readParser
  ["Alpha", "Beta", "DEBUG", "Master", "WILDCARD", "PERF"]

androidFlavor :: Parser Flavor
androidFlavor = readParser
  ["ALPHA", "Beta", "DEBUG", "PROFILE", "MASTER", "UIAUTOMATION",
   "UIAUTOMATIONDEBUG", "PERF", "WILDCARD", "NorthStar"]

variantParser :: Parser Variant
variantParser = "V/" *> readParser ["MUSHROOM", "SNAPSHOT", "OG"]

gaeSuffixParser :: Parser GAESuffix
gaeSuffixParser = do
  string "AppEngine-Google; (+http://code.google.com/appengine; appid: "
  projectID <- takeTill (== ')')
  char ')'
  return (AppEngine projectID)

iosParser :: Parser UserAgent
iosParser = do
  string "Snapchat/"
  version <- versionParser
  skipSpace
  flavor <- option Nothing (Just <$> iosFlavor)
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
  suffix <- option Nothing (Just <$> (skipSpace *> gaeSuffixParser))
  return (UserAgent version flavor device (IOS os) Nothing suffix)

androidParser :: Parser UserAgent
androidParser = do
  string "Snapchat/"
  version <- versionParser
  skipSpace
  flavor <- option Nothing (Just <$> androidFlavor)
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
  variant <- option Nothing (Just <$> (skipSpace *> variantParser))
  suffix <- option Nothing (Just <$> (skipSpace *> gaeSuffixParser))
  return (UserAgent version flavor device (Android os) variant suffix)
