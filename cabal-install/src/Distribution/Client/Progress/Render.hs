{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Distribution.Client.Progress.Render
  ( redraw, totalLine, componentLine, componentLines
  , ColumnWidths, defaultColumnWidths, Align(..), align
  , progressBar, ratio, filledSegments
  , moveCursorLinesUp, clearToEndOfScreen
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe)

import Distribution.Client.Progress.Types

-- | Deletes any previously drawn lines and draws the new set of lines in place.
redraw :: Int    -- ^ Previous line count (required to be deleted before we redraw the content)
       -> [Text] -- ^ New lines to be drawn.
       -> IO ()
redraw prevLineCount newLines = do
  TIO.putStr $ moveCursorLinesUp prevLineCount
  TIO.putStr clearToEndOfScreen
  mapM_ TIO.putStrLn newLines

data ProgressBarConfig =
     ProgressBarConfig { pbcWidth            :: Int  -- ^ Width in characters on terminal.
                       , pbcIncompleteVisual :: Text -- ^ Visual displayed as incomplete part.
                       , pbcCompletedVisual  :: Text -- ^ Visual displayed as completed part.
                       } deriving Show

defaultTotalBar :: ProgressBarConfig
defaultTotalBar =  ProgressBarConfig 40 (T.singleton '\x25B1') (T.singleton '\x25B0')

totalLine :: ColumnWidths -> Int -> Int -> Text
totalLine ColumnWidths {..} done total = align ToLeft cwAggregate "total " <>
                                         align ToLeft cwRatio (ratio cwRatioDigits done total <> " ") <> "   " <>
                                         progressBar defaultTotalBar done total
                                        
componentLine :: ColumnWidths
              -> PackageName
              -> ComponentEvent -- ^ When component has moved to compile next module or completed compilation.
              -> Maybe Text     -- ^ Component progress line.
componentLine ColumnWidths {..} packageName ce = case ce of
                                       CompilingModule {..} -> Just $ align ToLeft cwAggregate (packageName <> " ") <>
                                                                               ratio cwRatioDigits ceDoneCount ceTotalCount <> " " <> "   " <>
                                                                               align ToLeft cwBarOrModule ceModuleName
                                       ComponentDone -> Nothing

type PackageName = Text

componentLines :: [(PackageName, ComponentEvent)] -> [Text]
componentLines = mapMaybe (uncurry $ componentLine defaultColumnWidths)

data ColumnWidths =
     ColumnWidths { cwAggregate   :: Int
                  , cwRatio       :: Int
                  , cwRatioDigits :: Int
                  , cwBarOrModule :: Int -- ^ Should be `pbcWidth`
                  } deriving Show

defaultColumnWidths :: ColumnWidths
defaultColumnWidths =  ColumnWidths 15 10 3 (pbcWidth defaultTotalBar)

data Align = ToLeft
           | ToRight deriving (Show, Eq)

align :: Align -> Int -> Text -> Text
align to width t = let filling = T.replicate (max 0 $ width - T.length t) " " in
                   case to of
                     ToLeft  -> t <> filling
                     ToRight -> filling <> t

progressBar :: ProgressBarConfig -> Int -> Int -> Text
progressBar ProgressBarConfig {..} doneCount totalCount = let filled = filledSegments pbcWidth doneCount totalCount in
                                                             T.replicate filled pbcCompletedVisual <> 
                                                             T.replicate (pbcWidth - filled) pbcIncompleteVisual

ratio :: Int  -- ^ Digits of each number to align to.
      -> Int  -- ^ Done count.
      -> Int  -- ^ Total count.
      -> Text -- ^ Formatted ratio.
ratio digits done total = "[" <>
                          align ToRight digits (T.pack $ show done) <>
                          " / " <>
                          align ToRight digits (T.pack $ show total) <>
                          "]"

filledSegments :: Int -- ^ Target width of progress bar.
               -> Int -- ^ Done count of components.
               -> Int -- ^ Total count of components.
               -> Int -- ^ Segment count filled.
filledSegments width done total = floor $ (fromIntegral done) / (fromIntegral total) * fromIntegral width 

moveCursorLinesUp :: Int -> Text
moveCursorLinesUp n | n <= 0    = ""
                    | otherwise = "\x1b[" <> T.pack (show n) <> "F"

clearToEndOfScreen :: Text
clearToEndOfScreen = "\ESC[J"
