{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BookEntry (BookEntry)
import Codec.Archive.Zip (Entry, findEntryByPath, fromEntry, toArchiveOrFail)
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict)
import Data.Bool (otherwise)
import qualified Data.ByteString.Lazy as BL
import Data.Either (Either (Left, Right))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing), mapMaybe)
import Data.Semigroup ((<>))
import Data.String (IsString, String)
import Data.Text (Text, drop, isPrefixOf, length, lines, replace)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Read (decimal)
import Data.Version (showVersion)
import Paths_openreads_to_storygraph (version)
import System.IO (IO, print, putStrLn)
import Text.Show (show)
import Prelude ()

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

parseBooksFile :: Entry -> Either String [BookEntry]
parseBooksFile entry =
  let uncompressedEntry :: BL.ByteString
      uncompressedEntry = fromEntry entry
      makeArray x = "[" <> x <> "]"
      elements :: Either String [BookEntry]
      elements = eitherDecodeStrict (encodeUtf8 (makeArray (replace "@@@@@" "," (decodeUtf8 (BL.toStrict uncompressedEntry)))))
   in case elements of
        Left e -> Left $ "one of the books couldn't be decoded: " <> e
        Right v -> Right v

main :: IO ()
main = do
  let archiveFile = "test-backup.zip"
  compressedBsContents <- BL.readFile archiveFile
  case toArchiveOrFail compressedBsContents of
    Left errorMessage -> outputError $ "error reading from archive “" <> archiveFile <> "” (maybe it’s not a proper backup file?): " <> errorMessage
    Right archive ->
      case findEntryByPath magicInfoFile archive of
        Nothing ->
          outputError $
            "error reading from archive “"
              <> archiveFile
              <> "” (maybe it’s not a proper backup file?): couldn't find magic file “"
              <> magicInfoFile
              <> "”"
        Just magicInfoFileEntry ->
          case parseMagicInfoFileBackupVersion magicInfoFileEntry of
            Left e ->
              outputError $
                "error reading magic info file “"
                  <> magicInfoFile
                  <> "” from archive “"
                  <> archiveFile
                  <> "” (maybe it’s not a proper backup file?): "
                  <> e
            Right backupVersion -> do
              when (backupVersion /= testedBackupVersion) do
                putStrLn $
                  "this backup is version "
                    <> show backupVersion
                    <> " but we have tested the progam only until version "
                    <> show testedBackupVersion
                    <> ", expect problems"
              case findEntryByPath magicBooksFile archive of
                Nothing ->
                  outputError $
                    "error reading from archive “"
                      <> archiveFile
                      <> "” (maybe it’s not a proper backup file?): couldn't find magic file “"
                      <> magicBooksFile
                      <> "”"
                Just booksFile ->
                  case parseBooksFile booksFile of
                    Left e ->
                      outputError $
                        "error reading magic books file “"
                          <> magicBooksFile
                          <> "” from archive “"
                          <> archiveFile
                          <> "” (maybe it’s not a proper backup file?): "
                          <> e
                    Right bookList -> print bookList
