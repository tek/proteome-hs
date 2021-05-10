module Proteome.Init where

import Control.Monad.Catch (MonadThrow)
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Api.Autocmd (uautocmd)
import Ribosome.Config.Setting (settingMaybe, updateSetting)
import Ribosome.Control.Monad.Ribo (RNeovim, runRibo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report (reportError')
import Ribosome.Internal.IO (retypeNeovim)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Proteome.Config (logConfig, readConfig)
import Proteome.Data.Env (Env, Proteome)
import qualified Proteome.Data.Env as Env (mainProject)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata(VirtualProject, DirProject))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Data.ResolveError (ResolveError)
import Proteome.PersistBuffers (loadBuffers)
import Proteome.Project (pathData)
import Proteome.Project.Activate (activateProject)
import Proteome.Project.Resolve (resolveProjectFromConfig)
import qualified Proteome.Settings as Settings (mainName, mainProjectDir, mainType)
import Ribosome.Log (showDebug)

resolveMainProject ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  m Project
resolveMainProject = do
  mainDir <- settingMaybe Settings.mainProjectDir
  vimCwd <- vimCallFunction "getcwd" []
  let (root, name, tpe) = pathData (fromMaybe vimCwd mainDir)
  resolveProjectFromConfig (Just root) name (Just tpe)

setMainProject ::
  MonadDeepState s Env m =>
  Project ->
  m ()
setMainProject =
  setL @Env Env.mainProject

updateMainType ::
  NvimE e m =>
  MonadRibo m =>
  Maybe ProjectType ->
  m ()
updateMainType tpe =
  updateSetting Settings.mainType (fromMaybe (ProjectType "none") tpe)

setMainProjectVars ::
  NvimE e m =>
  MonadRibo m =>
  ProjectMetadata ->
  m ()
setMainProjectVars (DirProject name _ tpe) = do
  updateSetting Settings.mainName name
  updateMainType tpe
setMainProjectVars (VirtualProject name) = do
  updateSetting Settings.mainName name
  updateMainType (Just (ProjectType "virtual"))

initWithMain ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  Project ->
  m ()
initWithMain main@(Project meta _ _ _) = do
  showDebug "initializing with main project:" main
  setMainProject main
  setMainProjectVars meta
  activateProject main

resolveAndInitMain ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  MonadDeepState s Env m =>
  m ()
resolveAndInitMain =
  initWithMain =<< resolveMainProject

initialize' ::
  RNeovim Env (Ribosome Env)
initialize' = do
  result <- runRibo (resolveAndInitMain :: Proteome ())
  reportError' "init" result
  asks' customConfig

initialize :: Neovim e (Ribosome Env)
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "proteome" def
  retypeNeovim (const ribo) initialize'

proteomeStage1 ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  m ()
proteomeStage1 =
  loadBuffers

loadConfig ::
  NvimE e m =>
  MonadDeepState s Env m =>
  Text ->
  m ()
loadConfig dir =
  logConfig =<< readConfig dir =<< getL @Env Env.mainProject

proteomeStage2 ::
  NvimE e m =>
  MonadDeepState s Env m =>
  m ()
proteomeStage2 =
  loadConfig "project" *>
  uautocmd True "ProteomeProject"

proteomeStage4 ::
  NvimE e m =>
  MonadDeepState s Env m =>
  m ()
proteomeStage4 =
  loadConfig "project_after" *>
  uautocmd True "ProteomeProjectAfter"
