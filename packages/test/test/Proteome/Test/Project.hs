module Proteome.Test.Project where

import Control.Monad.Catch (MonadThrow)
import Path (parseRelDir, (</>))
import Path.IO (ensureDir)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.SettingError (SettingError)

import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Data.ProjectLang (ProjectLang(ProjectLang))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import qualified Proteome.Settings as Settings (projectConfig)

flag :: Text
flag = "flagellum"

hask :: Text
hask = "haskell"

idr :: Text
idr = "idris"

ag :: Text
ag = "agda"

cil :: Text
cil = "cilia"

prot :: Text
prot = "proteome"

fn :: ProjectName
fn = ProjectName flag

cn :: ProjectName
cn = ProjectName cil

tp :: ProjectType
tp = ProjectType hask

ti :: ProjectType
ti = ProjectType idr

ta :: ProjectType
ta = ProjectType ag

l :: ProjectLang
l = ProjectLang hask

li :: ProjectLang
li = ProjectLang idr

la :: ProjectLang
la = ProjectLang ag

createTestProject ::
  NvimE e m =>
  MonadFail m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  ProjectType ->
  ProjectName ->
  m ()
createTestProject (ProjectType tpe) (ProjectName name) = do
  (ProjectConfig (base : _) _ _ _ _ _ _) <- setting Settings.projectConfig
  typePath <- parseRelDir (toString tpe)
  namePath <- parseRelDir (toString name)
  ensureDir (base </> typePath </> namePath)
