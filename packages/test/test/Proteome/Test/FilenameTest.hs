module Proteome.Test.FilenameTest where

import Hedgehog ((===))
import Path (File, Path, Rel, parseAbsDir, reldir, relfile, stripProperPrefix, toFilePath, (</>))
import Path.IO (doesFileExist, getCurrentDir, listDir)
import Ribosome.Api.Buffer (currentBufferName, edit)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (persistenceDir)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Unit (tempDir)
import Test.Tasty (TestTree, testGroup)

import Proteome.Data.Env (Proteome)
import Proteome.Filename (proCopy, proMove, proRemove)
import Proteome.Path (pathText)
import Proteome.Test.Unit (ProteomeTest, tmuxTest)

filenameTest ::
  (Text -> Proteome ()) ->
  Path Rel File ->
  ProteomeTest Bool
filenameTest cmd changedRel = do
  cwd <- getCurrentDir
  base <- parseAbsDir =<< tempDir "rename"
  baseRel <- pathText <$> stripProperPrefix cwd base
  let
    initial = base </> [relfile|File.hs|]
    changed = base </> changedRel
  edit (toFilePath initial)
  vimCommand "write"
  lift (cmd baseRel)
  vimCommand "write"
  (True ===) =<< doesFileExist changed
  (pathText changed ===) =<< currentBufferName
  doesFileExist initial

moveRelDirTest :: ProteomeTest ()
moveRelDirTest = do
  initialExists <- filenameTest (\ b -> proMove [text|#{b}/sub/dir/|]) [relfile|sub/dir/File.hs|]
  False === initialExists

test_moveRelDir :: UnitTest
test_moveRelDir =
  tmuxTest moveRelDirTest

moveRenameTest :: ProteomeTest ()
moveRenameTest = do
  initialExists <- filenameTest (\ _ -> proMove "Changed") [relfile|Changed.hs|]
  False === initialExists

test_moveRename :: UnitTest
test_moveRename =
  tmuxTest moveRenameTest

copyRenameTest :: ProteomeTest ()
copyRenameTest = do
  initialExists <- filenameTest (\ _ -> proCopy "Changed") [relfile|Changed.hs|]
  True === initialExists

test_copyRename :: UnitTest
test_copyRename =
  tmuxTest copyRenameTest

removeTest :: ProteomeTest ()
removeTest = do
  persistDir <- parseAbsDir =<< tempDir "rename/persist"
  updateSetting persistenceDir (toFilePath persistDir)
  base <- parseAbsDir =<< tempDir "rename"
  let
    initial = base </> [relfile|File.hs|]
  edit (toFilePath initial)
  vimCommand "write"
  lift proRemove
  ("" ===) =<< currentBufferName
  (True ===) . not =<< doesFileExist initial
  trash <- listDir (persistDir </> [reldir|proteome/trash|])
  1 === length trash

test_remove :: UnitTest
test_remove = do
  tmuxTest removeTest

test_filename :: TestTree
test_filename =
  testGroup "filenames" [
    unitTest "move a file into a relative dir" test_moveRelDir,
    unitTest "rename a file" test_moveRename,
    unitTest "copy a file" test_copyRename,
    unitTest "remoe a file" test_remove
  ]
