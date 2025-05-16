{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenreadsEntry (OpenreadsEntry (..), OpenreadsReading (..), openreadsDay, extractOpenreadsReadingList) where

import Control.Applicative (pure, (<*>))
import Control.Monad (fail)
import Data.Aeson (FromJSON, parseJSON, withText)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.Text (Text, splitOn, unpack)
import Data.Time (UTCTime, utctDay)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (traverse)
import GHC.Generics (Generic)
import Text.Show (Show)
import Prelude ()

data OpenreadsReading = OpenreadsReading (Maybe UTCTime) (Maybe UTCTime) deriving (Show)

parseTime = iso8601ParseM . unpack . (<> "Z")

newtype OpenreadsReadingList = OpenreadsReadingList [OpenreadsReading] deriving (Show)

extractOpenreadsReadingList (OpenreadsReadingList l) = l

parseReading part =
  case splitOn "|" part of
    [""] -> pure (OpenreadsReading Nothing Nothing)
    ["", to, ""] -> OpenreadsReading Nothing <$> (Just <$> parseTime to)
    [from, "", ""] -> OpenreadsReading <$> Just <$> parseTime from <*> pure Nothing
    [from, to, ""] -> OpenreadsReading <$> (Just <$> parseTime from) <*> (Just <$> parseTime to)
    _ -> fail $ "didn't get <start>|<end>|: " <> unpack part

instance FromJSON OpenreadsReadingList where
  parseJSON = withText "Reading" \v ->
    OpenreadsReadingList <$> traverse parseReading (splitOn ";" v)

newtype OpenreadsTime = OpenreadsTime UTCTime deriving (Show)

openreadsDay (OpenreadsTime d) = utctDay d

instance FromJSON OpenreadsTime where
  parseJSON = withText "openreads date/time" \v -> OpenreadsTime <$> parseTime v

-- Examples:
-- "2025-03-17T00:00:00.000|2025-05-04T00:00:00.000|"
-- "2025-05-04T00:00:00.000||"
-- "|2022-05-23T00:00:00.000|"
instance FromJSON OpenreadsReading where
  parseJSON = withText "Reading" \v ->
    case splitOn "|" v of
      [""] -> pure (OpenreadsReading Nothing Nothing)
      ["", to, ""] -> OpenreadsReading Nothing <$> (Just <$> parseTime to)
      [from, "", ""] -> OpenreadsReading <$> Just <$> parseTime from <*> pure Nothing
      [from, to, ""] -> OpenreadsReading <$> (Just <$> parseTime from) <*> (Just <$> parseTime to)
      _ -> fail $ "didn't get <start>|<end>|: " <> unpack v

data OpenreadsEntry = OpenreadsEntry
  { id :: Int,
    title :: Text,
    author :: Text,
    subtitle :: Maybe Text,
    description :: Maybe Text,
    status :: Int,
    rating :: Maybe Int,
    favourite :: Int,
    deleted :: Int,
    pages :: Maybe Int,
    publication_year :: Maybe Int,
    isbn :: Text,
    olid :: Maybe Text,
    -- Here I'm not sure, is it an array?
    tags :: Maybe Text,
    my_review :: Maybe Text,
    notes :: Maybe Text,
    has_cover :: Int,
    book_type :: Text,
    readings :: OpenreadsReadingList,
    date_added :: OpenreadsTime,
    date_modified :: OpenreadsTime
  }
  deriving (Show, Generic)

instance FromJSON OpenreadsEntry
