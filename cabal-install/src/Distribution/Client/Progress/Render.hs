{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Distribution.Client.Progress.Render
  ( redraw, renderState, totalLine, componentLine, componentLines
  , ColumnWidths, defaultColumnWidths, Align(..), align
  , progressBar, ratio, filledSegments
  , moveCursorLinesUp, clearToEndOfScreen
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe)
import System.IO (hFlush, stdout, Handle)
import qualified Data.Map.Strict as M

import Distribution.Types.Progress
import Distribution.Pretty          (prettyShow)
import Distribution.Types.PackageId (PackageIdentifier)

redrawFrames :: Int -> [Text] -> Text
redrawFrames prevLines newLines =
  moveCursorLinesUp prevLines
    <> clearToEndOfScreen
    <> mconcat (map (<> "\n") newLines)

-- | Deletes any previously drawn lines and draws the new set of lines in place.
redraw :: Handle -> Int -> [Text] -> IO ()
redraw h prevLines newLines = do
  TIO.hPutStr h (redrawFrames prevLines newLines)
  hFlush h

renderState :: M.Map PackageIdentifier ComponentEvent
            -> Int    -- ^ done count
            -> Int    -- ^ total count
            -> [Text]
renderState eventMap doneCount totalCount =
  totalLine defaultColumnWidths doneCount totalCount
    : componentLines (M.assocs eventMap)

data ProgressBarConfig =
     ProgressBarConfig { pbcWidth            :: Int  -- ^ Width in characters on terminal.
                       , pbcIncompleteVisual :: Text -- ^ Visual displayed as incomplete part.
                       , pbcCompletedVisual  :: Text -- ^ Visual displayed as completed part.
                       } deriving Show

defaultTotalBar :: ProgressBarConfig
defaultTotalBar =  ProgressBarConfig 40 (T.singleton '\x25B1') (T.singleton '\x25B0')

totalLine :: ColumnWidths -> Int -> Int -> Text
totalLine ColumnWidths{..} done total =
  align ToLeft cwName "total"
    <> " " <> align ToLeft cwVersion ""
    <> " " <> align ToRight cwPhase ""
    <> "  " <> align ToRight cwRatio (ratio cwRatioDigits done total)
    <> "   "
    <> progressBar defaultTotalBar done total

splitPkgId :: PackageIdentifier -> (Text, Text)
splitPkgId pkgid =
  let t = T.pack (prettyShow pkgid)
      (verRev, nameRev) = T.span (/= '-') (T.reverse t)
  in (T.reverse (T.drop 1 nameRev), T.reverse verRev)

componentLine :: ColumnWidths
              -> PackageIdentifier
              -> ComponentEvent -- ^ When component has moved to compile next module or completed compilation.
              -> Maybe Text     -- ^ Component progress line.
componentLine ColumnWidths{..} pkgid ce = 
  let pkgStr = T.pack (prettyShow pkgid)
      (nameWithDash, ver) = T.breakOnEnd "-" pkgStr
      nameClean = if T.null nameWithDash then pkgStr else T.dropEnd 1 nameWithDash
      verClean  = if T.null nameWithDash then "" else ver
      
      base = align ToLeft cwName nameClean
          <> " " <> align ToLeft cwVersion verClean
          <> " " 
          
      phaseText str = align ToRight cwPhase str
      blankTail = "  " <> align ToRight cwRatio "" 
                  <> "   " <> align ToLeft cwBarOrModule ""
  in case ce of
    Downloading -> Just $ base <> phaseText "Downloading" <> blankTail
    Downloaded  -> Just $ base <> phaseText "Downloaded"  <> blankTail
    Configuring -> Just $ base <> phaseText "Configuring" <> blankTail
    Building    -> Just $ base <> phaseText "Building"    <> blankTail
    CompilingModule{..} ->
      Just $ base <> phaseText "Compiling"
           <> "  " <> align ToRight cwRatio (ratio cwRatioDigits ceDoneCount ceTotalCount)
           <> "   " <> align ToLeft cwBarOrModule ceModuleName
    Installing  -> Just $ base <> phaseText "Installing"  <> blankTail
    ComponentDone -> Just $ base <> phaseText "Done"      <> blankTail
    ComponentFailed reason -> Just $ base <> phaseText "Failed" <> blankTail

componentLines :: [(PackageIdentifier, ComponentEvent)] -> [Text]
componentLines = mapMaybe (uncurry $ componentLine defaultColumnWidths)

data ColumnWidths = ColumnWidths
  { cwName        :: Int  -- ^ Package name
  , cwVersion     :: Int  -- ^ Version
  , cwPhase       :: Int
  , cwRatio       :: Int  -- ^ Ratio string
  , cwRatioDigits :: Int
  , cwBarOrModule :: Int
  } deriving Show

defaultColumnWidths :: ColumnWidths
defaultColumnWidths =  ColumnWidths 22 8 12 11 3 (pbcWidth defaultTotalBar)

data Align = ToLeft
           | ToRight deriving (Show, Eq)

align :: Align -> Int -> Text -> Text
align to width t =
  let t' = if T.length t > width then T.take width t else t
      filling = T.replicate (max 0 (width - T.length t')) " "
  in case to of
       ToLeft  -> t' <> filling
       ToRight -> filling <> t'

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
