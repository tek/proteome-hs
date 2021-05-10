module Proteome.Test.InitTest where

import Hedgehog ((===))
import Ribosome.Config.Setting (setting)
import Ribosome.Nvim.Api.IO (vimCommand, vimGetVar)
import Ribosome.Test.Run (UnitTest)

import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Init (proteomeStage2, resolveAndInitMain)
import qualified Proteome.Settings as Settings
import Proteome.Test.Config (vars)
import Proteome.Test.Unit (ProteomeTest, testWithDef)

initSpec :: ProteomeTest ()
initSpec = do
  vimCommand "autocmd User ProteomeProject let g:success = 13"
  resolveAndInitMain
  proteomeStage2
  tpe <- setting Settings.mainType
  name <- setting Settings.mainName
  ProjectName "flagellum" === name
  ProjectType "haskell" === tpe
  ((13 :: Int) ===) =<< vimGetVar "success"

test_init :: UnitTest
test_init =
  vars >>= testWithDef initSpec
