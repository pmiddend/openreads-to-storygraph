{-# LANGUAGE DeriveGeneric #-}

module BookEntry (BookEntry (..)) where

import Data.Aeson (FromJSON)
import Data.Int (Int)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)
import Prelude ()

data BookEntry = BookEntry
  { id :: Int,
    title :: Text,
    author :: Text,
    isbn :: Text
  }
  deriving (Show, Generic)

instance FromJSON BookEntry
