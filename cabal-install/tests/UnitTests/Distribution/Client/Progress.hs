module UnitTests.Distribution.Client.Progress (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec

import Distribution.Client.Progress.Types
import Distribution.Client.Progress.Render   (filledSegments)
import Distribution.Client.Progress.Parser   (rectBracket, ghcLine)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [progress, ghcLineParser]

progress = testGroup "progress lib"
 [ testCase "filledSegments computes scaled filled count correctly" $ do
     filledSegments 40 2 10 @?= 8
     filledSegments 40 1 80 @?= 0
     filledSegments 40 2 80 @?= 1
     filledSegments 40 79 80 @?= 39
     filledSegments 40 80 80 @?= 40
 ]

ghcLineParser = testGroup "ghc line parser"
 [ testCase "rectBracket parser `[x of n]` correctly" $ do
    parse rectBracket "" "[1 of 3]"   @?= Right (1,3)
    parse rectBracket "" "[1 of 99]"  @?= Right (1,99)
    parse rectBracket "" "[22 of 99]" @?= Right (22,99)
  -- testCase "parseGHCLine" $ do
     -- parseGHCLine "[1 of 3] Compiling Some.Fancy.Module.Name" @?= Just "MovedToCompileNextModule "
  , testCase "ghcLine parses lines correctly: [1 of 3]" $ do
      parse ghcLine "" "[1 of 3] Compiling Some.Fancy.Module other"
      @?= Right (CompilingModule {ceDoneCount = 0, ceTotalCount = 3, ceModuleName = "Some.Fancy.Module"})
  , testCase "ghcLine parses lines correctly: [3 of 3]" $ do
      parse ghcLine "" "[3 of 3] Compiling Some.Fancy.Module other"
      @?= Right (CompilingModule {ceDoneCount = 2, ceTotalCount = 3, ceModuleName = "Some.Fancy.Module"})
   ]

