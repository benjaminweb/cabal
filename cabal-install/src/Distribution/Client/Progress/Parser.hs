{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Progress.Parser
 ( rectBracket
 , ghcLineParser
 , progressEventParser
 , parseGhcLine
 ) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isAlphaNum)
import Control.Monad (void)

import Distribution.Types.Progress
import Distribution.Types.PackageId (PackageIdentifier)
import Distribution.Parsec          (simpleParsec)

rectBracket :: Parser (Int, Int)
rectBracket = do
  void $ char '[' <* spaces
  x <- read <$> many1 digit
  spaces
  void $ string "of"
  spaces
  n <- read <$> many1 digit
  spaces
  void $ char ']'
  return (x, n)

ghcLineParser :: Parser ComponentEvent
ghcLineParser = do
  ( ceStartedCount
   , ceTotalCount ) <- rectBracket
  let ceDoneCount = ceStartedCount - 1
  spaces
  void $ string "Compiling"
  spaces
  ceModuleName <- T.pack <$> many1 (satisfy (not . isSpace))
  spaces
  return $ CompilingModule {..}

-- | Cabal's own 'PackageIdentifier' parser, bridged into 'Text.Parsec'
-- via 'simpleParsec'. Fails Parsec-style (so 'progressEvent' can 'fail')
-- on invalid package identifiers.
packageIdentifier :: Parser PackageIdentifier
packageIdentifier = do
  cs <- many1 (satisfy (\c -> isAlphaNum c || c == '_' || c == '-' || c == '.'))
  case simpleParsec cs of
    Just pid -> pure pid
    Nothing  -> fail ("not a package identifier: " ++ cs)

progressEventParser :: Parser ProgressEvent
progressEventParser = do
  pkgid  <- packageIdentifier
  void $ string ": "
  ProgressEvent pkgid <$> ghcLineParser

parseGhcLine :: Text -> Maybe ProgressEvent
parseGhcLine = either (const Nothing) Just . parse progressEventParser ""

