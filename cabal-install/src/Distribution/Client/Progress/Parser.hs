module Distribution.Client.Progress.Parser (parseGhcLine) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Char (isSpace)
import qualified Data.Text as T
import Control.Monad (void)

import Distribution.Client.Progress.Types

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

ghcLine :: Parser ComponentEvent
ghcLine = do
  ( ceStartedCount
   , ceTotalCount ) <- rectBracket
  let ceDoneCount = ceStartedCount - 1
  spaces
  void $ string "Compiling"
  spaces
  ceModuleName <- T.pack <$> many1 (satisfy (not . isSpace))
  spaces
  return $ CompilingModule {..}
