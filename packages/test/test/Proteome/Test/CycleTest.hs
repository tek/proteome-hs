module Proteome.Test.CycleTest where

import Control.Monad.Catch (MonadThrow)
import Hedgehog (TestT, (===))
import Path (Abs, Dir, Path, parseAbsDir, parseRelDir, (</>))
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)

import Proteome.Add (proAdd)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Init (resolveAndInitMain)
import Proteome.Project.Activate (proNext, proPrev)
import qualified Proteome.Settings as Settings (mainProjectDir, projectConfig)
import Proteome.Test.Config (vars)
import Proteome.Test.Project (cil, cn, flag, fn, hask, prot, tp)
import Proteome.Test.Unit (ProteomeTest, testWithDef)

assertProject ::
  NvimE e m =>
  MonadThrow m =>
  Path Abs Dir ->
  Text ->
  TestT m ()
assertProject projectsDir n = do
  cwd <- parseAbsDir =<< nvimCwd
  haskPath <- parseRelDir (toString hask)
  nPath <- parseRelDir (toString n)
  projectsDir </> haskPath </> nPath === cwd

cycleTest :: ProteomeTest ()
cycleTest = do
  projectsDir <- parseAbsDir =<< fixture "projects"
  let assertDir = assertProject projectsDir
  haskPath <- parseRelDir (toString hask)
  protPath <- parseRelDir (toString prot)
  let mainDir = projectsDir </> haskPath </> protPath
  updateSetting Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
  updateSetting Settings.mainProjectDir mainDir
  resolveAndInitMain
  proAdd $ AddOptions fn tp (Just False)
  proAdd $ AddOptions cn tp (Just False)
  assertDir prot
  let checkNext n = proNext *> assertDir n
  traverse_ @[] checkNext [flag, cil, prot, flag]
  proPrev
  assertDir prot

test_cycle :: UnitTest
test_cycle =
  vars >>= testWithDef cycleTest
