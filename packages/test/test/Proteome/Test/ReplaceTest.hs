module Proteome.Test.ReplaceTest where

import Conduit (ConduitT, yieldMany)
import qualified Data.Text as Text (replace)
import Hedgehog (TestT, (===))
import Ribosome.Api.Buffer (buflisted, currentBufferContent, setCurrentBufferContent)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Ribosome.Nvim.Api.IO (vimGetBuffers)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (tempDir)
import Text.RE.PCRE.Text (ed, (*=~/))

import Proteome.Grep (proGrepWith)
import Proteome.Grep.Replace (proReplaceQuit, proReplaceSave)
import Proteome.Test.Unit (ProteomeTest, tmuxTest)

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
  "target"

replacement :: Text
replacement =
  "replaced"

replaceChars :: [Text]
replaceChars =
  ["*", "r"]

file1Lines :: [Text]
file1Lines =
  [
    "garbage 0",
    "target",
    "garbage 1",
    "and target here"
  ]

file2Lines :: [Text]
file2Lines =
  [
    "garbage 2",
    "and target"
  ]

file3Lines :: [Text]
file3Lines =
  [
    "delete me target 1",
    "",
    "keep me 1",
    "",
    "delete me target 2",
    "",
    "keep me 2",
    "delete me target 3",
    "keep me 3",
    "",
    "keep me 4",
    "delete me target 4",
    ""
  ]

file1Target :: [Text]
file1Target =
  [
    "garbage 0",
    "replaced",
    "garbage 1",
    "and replaced here"
  ]

file2Target :: [Text]
file2Target =
  [
    "garbage 2",
    "and replaced"
  ]

file3Target :: [Text]
file3Target =
  [
    "keep me 1",
    "",
    "keep me 2",
    "keep me 3",
    "",
    "keep me 4"
  ]

checkContent ::
  MonadIO m =>
  FilePath ->
  [Text] ->
  FilePath ->
  TestT m ()
checkContent dir target name =
  (target ===) . lines . toText =<< readFile (dir <> "/" <> name)

grepReplaceTest :: ProteomeTest ()
grepReplaceTest = do
  dir <- tempDir "grep/replace"
  writeFile (dir <> "/file1") (toString $ unlines file1Lines)
  writeFile (dir <> "/file2") (toString $ unlines file2Lines)
  writeFile (dir <> "/file3") (toString $ unlines file3Lines)
  proGrepWith (promptConfig replaceChars) (toText dir) pat []
  replaceContent <- currentBufferContent
  (7 ===) (length replaceContent)
  setCurrentBufferContent $ (*=~/ [ed|^delete me.*$///|]) . Text.replace pat replacement <$> replaceContent
  proReplaceSave
  proReplaceQuit
  (2 ===) . length =<< filterM buflisted =<< vimGetBuffers
  checkContent dir file1Target "file1"
  checkContent dir file2Target "file2"
  checkContent dir file3Target "file3"

test_grepReplace :: UnitTest
test_grepReplace =
  tmuxTest grepReplaceTest

deleteChars :: [Text]
deleteChars =
  ["k", "space", "k", "space", "d"]

deleteFile1Lines :: [Text]
deleteFile1Lines =
  [
    "target 0",
    "",
    "target 1",
    "",
    "garbage",
    "target 2",
    "target 4",
    "garbage"
  ]

deleteFile1Target :: [Text]
deleteFile1Target =
  [
    "target 0",
    "",
    "garbage",
    "target 2",
    "garbage"
  ]

grepDeleteTest :: ProteomeTest ()
grepDeleteTest = do
  dir <- tempDir "grep/delete"
  writeFile (dir <> "/file1") (toString $ unlines deleteFile1Lines)
  proGrepWith (promptConfig deleteChars) (toText dir) pat []
  checkContent dir deleteFile1Target "file1"

test_grepDelete :: UnitTest
test_grepDelete =
  tmuxTest grepDeleteTest
