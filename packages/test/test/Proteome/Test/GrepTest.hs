module Proteome.Test.GrepTest where

import Conduit (ConduitT, runConduit, sinkList, yield, yieldMany, (.|))
import Hedgehog ((===))
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Window (currentLine)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Unit (fixture)
import Test.Tasty (TestTree, testGroup)

import Proteome.Data.GrepOutputLine (GrepOutputLine(GrepOutputLine))
import Proteome.Grep (proGrepWith)
import Proteome.Grep.Line (uniqueGrepLines)
import Proteome.Grep.Process (grepMenuItems)
import Proteome.Test.Unit (ProteomeTest, testDef, tmuxTest)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars' =
  sleep 0.1 *>
  yieldMany (PromptEvent.Character <$> chars')

promptConfig ::
  MonadIO m =>
  [Text] ->
  PromptConfig m
promptConfig cs =
  PromptConfig (promptInput cs) basicTransition noPromptRenderer []

pat :: Text
pat =
  "target with spaces"

jumpChars :: [Text]
jumpChars =
  ["k", "cr"]

grepJumpTest :: ProteomeTest ()
grepJumpTest = do
  dir <- fixture "grep/pro"
  proGrepWith (promptConfig jumpChars) (toText dir) pat []
  l <- currentLine
  5 === l

test_grepJump :: UnitTest
test_grepJump =
  tmuxTest grepJumpTest

yankChars :: [Text]
yankChars =
  ["k", "y"]

grepYankTest :: ProteomeTest ()
grepYankTest = do
  dir <- fixture "grep/pro"
  proGrepWith (promptConfig yankChars) (toText dir) pat []
  normal "P"
  l <- currentBufferContent
  ["line 6 " <> pat, ""] === l

test_grepYank :: UnitTest
test_grepYank =
  tmuxTest grepYankTest

grepDuplicatesTest :: ProteomeTest ()
grepDuplicatesTest = do
  dir <- toText <$> fixture "grep/duplicates"
  outputDupes <- runConduit $ proc dir .| sinkList
  2 === length (join outputDupes)
  outputUnique <- runConduit $ proc dir .| uniqueGrepLines .| sinkList
  1 === length (join outputUnique)
  output3 <- runConduit $ yield (item 1) *> yield (item 2) .| uniqueGrepLines .| sinkList
  1 === length (join output3)
  where
    proc dir =
      grepMenuItems dir "rg" ["--vimgrep", "--no-heading", "target", dir]
    item col =
      [MenuItem (GrepOutputLine "/path/to/file" 0 (Just col) "target") "" ""]

test_grepDuplicates :: UnitTest
test_grepDuplicates =
  testDef grepDuplicatesTest

test_grep :: TestTree
test_grep =
  testGroup "grep" [
    unitTest "jump to a result" test_grepJump,
    unitTest "yank a result" test_grepYank,
    unitTest "filter duplicates" test_grepDuplicates
  ]
