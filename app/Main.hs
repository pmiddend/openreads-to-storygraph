{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Archive.Zip (Entry, findEntryByPath, fromEntry, toArchiveOrFail)
import Control.Applicative (pure)
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict)
import Data.Bool (otherwise)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Csv (encodeDefaultOrderedByName)
import Data.Either (Either (Left, Right))
import Data.Eq ((/=), (==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, listToMaybe, mapMaybe, maybe)
import Data.Semigroup ((<>))
import Data.String (IsString, String)
import Data.Text (Text, drop, isPrefixOf, length, lines, replace)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Read (decimal)
import Data.Time (utctDay)
import Data.Time.Calendar (Day)
import Data.Version (showVersion)
import GoodreadsEntry (GoodreadsEntry (GoodreadsEntry, author), additionalAuthors, authorLastFirst, averageRating, binding, bookId, bookshelves, bookshelvesWithPositions, dateAdded, dateRead, exclusiveShelf, isbn, isbn13, myRating, myReview, numberOfPages, originalPublicationYear, ownedCopies, privateNotes, publisher, readCount, spoiler, title, yearPublished)
import OpenreadsEntry (OpenreadsEntry (..), OpenreadsReading (OpenreadsReading), extractOpenreadsReadingList, openreadsDay)
import Paths_openreads_to_storygraph (version)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (FilePath, IO, putStrLn)
import Text.Show (show)
import Prelude (fromIntegral, (/))

openreadsToStorygraphVersion :: String
openreadsToStorygraphVersion = showVersion version -- VERSIONSTRING

outputError :: String -> IO ()
outputError = putStrLn

magicBooksFile :: (IsString a) => a
magicBooksFile = "books.backup"

magicInfoFile :: (IsString a) => a
magicInfoFile = "info.txt"

backupVersionLinePrefix :: (IsString a) => a
backupVersionLinePrefix = "Backup version: "

testedBackupVersion :: Int
testedBackupVersion = 5

parseMagicInfoFileBackupVersion :: Entry -> Either String Int
parseMagicInfoFileBackupVersion entry =
  let uncompressedEntry :: BL.ByteString
      uncompressedEntry = fromEntry entry
      parseBackupVersionLine :: Text -> Maybe Int
      parseBackupVersionLine line
        | backupVersionLinePrefix `isPrefixOf` line =
            case decimal (drop (length backupVersionLinePrefix) line) of
              Left _ -> Nothing
              Right (v, _) -> Just v
        | otherwise = Nothing
   in case mapMaybe parseBackupVersionLine (lines (decodeUtf8 (BL.toStrict uncompressedEntry))) of
        [] -> Left $ "couldn't find magic backup version line with a number following this prefix: " <> backupVersionLinePrefix
        (backupVersion : _) -> Right backupVersion

parseBooksFile :: Entry -> Either String [OpenreadsEntry]
parseBooksFile entry =
  let uncompressedEntry :: BL.ByteString
      uncompressedEntry = fromEntry entry
      makeArray x = "[" <> x <> "]"
      elements :: Either String [OpenreadsEntry]
      elements = eitherDecodeStrict (encodeUtf8 (makeArray (replace "@@@@@" "," (decodeUtf8 (BL.toStrict uncompressedEntry)))))
   in case elements of
        Left e -> Left $ "one of the books couldn't be decoded: " <> e
        Right v -> Right v

openreadsToGoodreads :: OpenreadsEntry -> Maybe GoodreadsEntry
openreadsToGoodreads or =
  if or.isbn == ""
    then Nothing
    else
      Just $
        GoodreadsEntry
          { bookId = or.id,
            title = or.title,
            author = or.author,
            authorLastFirst = or.author,
            additionalAuthors = "",
            isbn = "",
            isbn13 = or.isbn,
            myRating = maybe 0.0 (\r -> fromIntegral r / 10) or.rating,
            averageRating = 0.0,
            publisher = "",
            binding = or.book_type,
            numberOfPages = fromMaybe 0 or.pages,
            yearPublished = or.publication_year,
            originalPublicationYear = or.publication_year,
            dateRead =
              let convertReading :: OpenreadsReading -> Maybe Day
                  convertReading (OpenreadsReading _ end) = utctDay <$> end
               in listToMaybe $
                    mapMaybe convertReading (extractOpenreadsReadingList or.readings),
            dateAdded = Just (openreadsDay or.date_added),
            bookshelves = "",
            bookshelvesWithPositions = "",
            -- This, for some reason, is very important for the StoryGraph import to succeed
            exclusiveShelf = "read",
            myReview = fromMaybe "" or.my_review,
            spoiler = "",
            privateNotes = "",
            readCount = 1,
            ownedCopies = 1
          }

realMain :: FilePath -> IO ExitCode
realMain archiveFile = do
  compressedBsContents <- BL.readFile archiveFile
  case toArchiveOrFail compressedBsContents of
    Left errorMessage -> do
      outputError $ "error reading from archive “" <> archiveFile <> "” (maybe it’s not a proper backup file?): " <> errorMessage
      pure (ExitFailure 2)
    Right archive ->
      case findEntryByPath magicInfoFile archive of
        Nothing -> do
          outputError $
            "error reading from archive “"
              <> archiveFile
              <> "” (maybe it’s not a proper backup file?): couldn't find magic file “"
              <> magicInfoFile
              <> "”"
          pure (ExitFailure 3)
        Just magicInfoFileEntry ->
          case parseMagicInfoFileBackupVersion magicInfoFileEntry of
            Left e -> do
              outputError $
                "error reading magic info file “"
                  <> magicInfoFile
                  <> "” from archive “"
                  <> archiveFile
                  <> "” (maybe it’s not a proper backup file?): "
                  <> e
              pure (ExitFailure 4)
            Right backupVersion -> do
              when (backupVersion /= testedBackupVersion) do
                putStrLn $
                  "this backup is version "
                    <> show backupVersion
                    <> " but we have tested the progam only until version "
                    <> show testedBackupVersion
                    <> ", expect problems"
              case findEntryByPath magicBooksFile archive of
                Nothing -> do
                  outputError $
                    "error reading from archive “"
                      <> archiveFile
                      <> "” (maybe it’s not a proper backup file?): couldn't find magic file “"
                      <> magicBooksFile
                      <> "”"
                  pure (ExitFailure 5)
                Just booksFile ->
                  case parseBooksFile booksFile of
                    Left e -> do
                      outputError $
                        "error reading magic books file “"
                          <> magicBooksFile
                          <> "” from archive “"
                          <> archiveFile
                          <> "” (maybe it’s not a proper backup file?): "
                          <> e
                      pure (ExitFailure 6)
                    Right bookList -> do
                      let convertedBooks = mapMaybe openreadsToGoodreads bookList
                      BL8.putStr (encodeDefaultOrderedByName convertedBooks)
                      pure ExitSuccess

main :: IO ExitCode
main = do
  args <- getArgs
  case args of
    ["--version"] -> do
      putStrLn $ "openreads-to-storygraph " <> openreadsToStorygraphVersion
      pure ExitSuccess
    [fileName] -> realMain fileName
    _ -> do
      putStrLn "usage: openreads-to-storygraph <backup-file>"
      pure (ExitFailure 1)
