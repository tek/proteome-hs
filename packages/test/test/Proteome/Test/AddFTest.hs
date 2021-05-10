module Proteome.Test.AddFTest where

import qualified Data.Map as Map (fromList)
import Path (parseAbsDir, reldir, (</>))
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Control.Ribosome (newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimSetVar)
import Ribosome.Test.Await (awaitEqual_)
import Ribosome.Test.Embed (integrationTestDef)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)

import Proteome.Data.Env (Proteome)
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Plugin (plugin')
import Proteome.Test.Unit (ProteomeTest)

addTestN :: Proteome () -> ProteomeTest ()
addTestN request = do
  projectsDir <- parseAbsDir =<< fixture "projects"
  () <- vimSetVar "proteome_project_config" (toMsgpack $ ProjectConfig [projectsDir] def def def def def def)
  () <- lift request
  awaitEqual_ (projectsDir </> [reldir|haskell|] </> [reldir|cilia|]) (parseAbsDir =<< nvimCwd)

addTest :: Proteome () -> UnitTest
addTest request = do
  ribo <- newRibosome "proteome" def
  integrationTestDef (plugin' ribo) (addTestN request)

test_addFunction :: UnitTest
test_addFunction =
  addTest request
  where
    request =
      vimCallFunction "ProAddProject" [toMsgpack $ Map.fromList [
        ("name" :: Text, toMsgpack ("cilia" :: Text)),
        ("tpe", toMsgpack ("haskell" :: Text)),
        ("activate", toMsgpack True)
        ]]

test_addCommand :: UnitTest
test_addCommand =
  addTest request
  where
    request =
      vimCommand "ProAdd! haskell/cilia"
