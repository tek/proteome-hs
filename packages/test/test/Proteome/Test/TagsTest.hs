module Proteome.Test.TagsTest where

import Hedgehog ((===))
import Path (Abs, Dir, Path, parseAbsDir, relfile, (</>))
import Path.IO (doesFileExist)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (tempDir)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (mainProject)
import qualified Proteome.Data.Project as Project (lang, meta)
import Proteome.Data.ProjectLang (ProjectLang(ProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import qualified Proteome.Settings as S (tagsArgs, tagsCommand, tagsFork)
import Proteome.Tags (proTags)
import Proteome.Test.Config (vars)
import Proteome.Test.Unit (ProteomeTest, testWithDef)

main :: Path Abs Dir -> ProjectMetadata
main root = DirProject (ProjectName "flagellum") (ProjectRoot root) (Just (ProjectType "haskell"))

tagsTest :: ProteomeTest ()
tagsTest = do
  root <- parseAbsDir =<< tempDir "projects/haskell/flagellum"
  setL @Env (Env.mainProject . Project.meta) (main root)
  setL @Env (Env.mainProject . Project.lang) (Just (ProjectLang "idris"))
  updateSetting S.tagsCommand "touch"
  updateSetting S.tagsArgs "tags-{langsComma}"
  updateSetting S.tagsFork False
  proTags
  let tagsFile = root </> [relfile|tags-idris|]
  exists <- liftIO $ doesFileExist tagsFile
  True === exists

test_simpleTags :: UnitTest
test_simpleTags =
  vars >>= testWithDef tagsTest
