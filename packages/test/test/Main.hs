module Main where

import Proteome.Test.AddFTest (test_addCommand, test_addFunction)
import Proteome.Test.AddMenuTest (test_addMenu)
import Proteome.Test.AddTest (test_add)
import Proteome.Test.BuffersTest (test_buffers)
import Proteome.Test.ConfigTest (test_config)
import Proteome.Test.CycleTest (test_cycle)
import Proteome.Test.DiagTest (test_diag)
import Proteome.Test.FilenameTest (test_filename)
import Proteome.Test.FilesTest (test_files)
import Proteome.Test.GrepTest (test_grep)
import Proteome.Test.InitTest (test_init)
import Proteome.Test.MruTest (test_mru)
import Proteome.Test.MultiTagsTest (test_multi)
import Proteome.Test.PersistLoadTest (test_loadPersistedBuffers)
import Proteome.Test.PersistStoreTest (test_storeBuffers)
import Proteome.Test.ReplaceTest (test_grepDelete, test_grepReplace)
import Proteome.Test.ResolveTest (test_resolve)
import Proteome.Test.TagsTest (test_simpleTags)
import Ribosome.Test.Run (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)


tests :: TestTree
tests =
  testGroup "all" [
    unitTest "add a project via function call" test_addFunction,
    unitTest "add a project via command" test_addCommand,
    unitTest "add a project from the add menu" test_addMenu,
    unitTest "add a project" test_add,
    test_buffers,
    unitTest "read project-specific config" test_config,
    unitTest "cycle projects" test_cycle,
    unitTest "diagnosticts window" test_diag,
    test_filename,
    test_files,
    test_grep,
    unitTest "project initialization" test_init,
    unitTest "MRU buffers" test_mru,
    unitTest "generate tags for multiple projects" test_multi,
    unitTest "load persisted buffers" test_loadPersistedBuffers,
    unitTest "store buffer list" test_storeBuffers,
    unitTest "replace text in grep results" test_grepReplace,
    unitTest "delete text in grep results" test_grepDelete,
    test_resolve,
    unitTest "generate tags" test_simpleTags
  ]


main :: IO ()
main =
  defaultMain tests
