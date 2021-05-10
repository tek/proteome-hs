module Proteome.Test.AddTest where

import Hedgehog ((===))
import Path (parseAbsDir, parseRelDir, (</>))
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)

import Proteome.Add (proAdd)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (projects)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import qualified Proteome.Settings as Settings (projectConfig)
import Proteome.Test.Config (vars)
import Proteome.Test.Project (flag, fn, hask, l, tp)
import Proteome.Test.Unit (ProteomeTest, testWithDef)

addSpec :: ProteomeTest ()
addSpec = do
  projectsDir <- parseAbsDir =<< fixture "projects"
  updateSetting Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
  proAdd $ AddOptions fn tp (Just False)
  ps <- getL @Env Env.projects
  haskPath <- parseRelDir (toString hask)
  flagPath <- parseRelDir (toString flag)
  let root = projectsDir </> haskPath </> flagPath
  [Project (DirProject fn (ProjectRoot root) (Just tp)) [] (Just l) []] === ps

test_add :: UnitTest
test_add = vars >>= testWithDef addSpec
