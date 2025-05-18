{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GoodreadsEntry (GoodreadsEntry (..)) where

import Data.Csv (DefaultOrdered (headerOrder), ToNamedRecord (toNamedRecord), namedRecord, (.=))
import Data.Function ((.))
import Data.Int (Int)
import Data.Maybe (Maybe, maybe)
import Data.Text (Text, pack)
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (formatTime)
import qualified Data.Vector as V
import Text.Show (Show)
import Prelude (Float)

data GoodreadsEntry = GoodreadsEntry
  { bookId :: Int,
    title :: Text,
    author :: Text,
    authorLastFirst :: Text,
    additionalAuthors :: Text,
    isbn :: Text,
    isbn13 :: Text,
    myRating :: Float,
    averageRating :: Float,
    publisher :: Text,
    binding :: Text,
    numberOfPages :: Int,
    yearPublished :: Maybe Int,
    originalPublicationYear :: Maybe Int,
    dateRead :: Maybe Day,
    dateAdded :: Maybe Day,
    bookshelves :: Text,
    bookshelvesWithPositions :: Text,
    exclusiveShelf :: Text,
    myReview :: Text,
    spoiler :: Text,
    privateNotes :: Text,
    readCount :: Int,
    ownedCopies :: Int
  }
  deriving (Show)

formatDay :: Day -> Text
formatDay = pack . formatTime defaultTimeLocale "%Y/%m/%d"

instance ToNamedRecord GoodreadsEntry where
  toNamedRecord x =
    namedRecord
      [ "Book Id" .= x.bookId,
        "Title" .= x.title,
        "Author" .= x.author,
        "Author l-f" .= x.authorLastFirst,
        "Additional Authors" .= x.additionalAuthors,
        "ISBN" .= x.isbn,
        "ISBN13" .= x.isbn13,
        "My Rating" .= x.myRating,
        "Average Rating" .= x.averageRating,
        "Publisher" .= x.publisher,
        "Binding" .= x.binding,
        "Number of Pages" .= x.numberOfPages,
        "Year Published" .= x.originalPublicationYear,
        "Original Publication Year" .= x.originalPublicationYear,
        "Date Read" .= maybe "" formatDay x.dateRead,
        "Date Added" .= maybe "" formatDay x.dateAdded,
        "Bookshelves" .= x.bookshelves,
        "Bookshelves with positions" .= x.bookshelvesWithPositions,
        "Exclusive Shelf" .= x.exclusiveShelf,
        "My Review" .= x.myReview,
        "Spoiler" .= x.spoiler,
        "Private Notes" .= x.privateNotes,
        "Read Count" .= x.readCount,
        "Owned Copies" .= x.ownedCopies
      ]

instance DefaultOrdered GoodreadsEntry where
  headerOrder _ =
    V.fromList
      [ "Book Id",
        "Title",
        "Author",
        "Author l-f",
        "Additional Authors",
        "ISBN",
        "ISBN13",
        "My Rating",
        "Average Rating",
        "Publisher",
        "Binding",
        "Number of Pages",
        "Year Published",
        "Original Publication Year",
        "Date Read",
        "Date Added",
        "Bookshelves",
        "Bookshelves with positions",
        "Exclusive Shelf",
        "My Review",
        "Spoiler",
        "Private Notes",
        "Read Count",
        "Owned Copies"
      ]
