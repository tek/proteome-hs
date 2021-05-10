module Proteome.Project.Activate where

import Path (toFilePath)
import Path.IO (doesDirExist)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.IO (vimCommand)

import Proteome.Data.ActiveProject (ActiveProject(ActiveProject))
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (currentProjectIndex)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject, VirtualProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Project (allProjects, currentProject)
import qualified Proteome.Settings as S (active)

activeProject :: Project -> ActiveProject
activeProject (Project (DirProject name _ tpe) _ lang _) = ActiveProject name (fromMaybe (ProjectType "none") tpe) lang
activeProject (Project (VirtualProject name) _ lang _) = ActiveProject name (ProjectType "virtual") lang

activateDirProject ::
  NvimE e m =>
  MonadIO m =>
  ProjectMetadata ->
  m ()
activateDirProject (DirProject _ (ProjectRoot root) _) = do
  exists <- liftIO $ doesDirExist root
  when exists $ vimCommand $ "chdir " <> toText (toFilePath root)
activateDirProject _ = return ()

activateProject ::
  NvimE e m =>
  MonadRibo m =>
  Project ->
  m ()
activateProject project@(Project meta _ _ _) = do
  updateSetting S.active $ activeProject project
  activateDirProject meta

describeProject :: ProjectMetadata -> Text
describeProject (DirProject (ProjectName name) _ (Just (ProjectType tpe))) = tpe <> "/" <> name
describeProject (DirProject (ProjectName name) _ Nothing) = name
describeProject (VirtualProject (ProjectName name)) = name

echoProjectActivation ::
  NvimE e m =>
  Project ->
  m ()
echoProjectActivation (Project meta _ _ _) =
  vimCommand $ "echo 'activated project " <> describeProject meta <> "'"

activateCurrentProject ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  m ()
activateCurrentProject = do
  pro <- currentProject
  mapM_ activateProject pro
  mapM_ echoProjectActivation pro

setProjectIndex ::
  MonadDeepState s Env m =>
  Int ->
  m ()
setProjectIndex index = do
  pros <- allProjects
  setL @Env Env.currentProjectIndex $ index `mod` length pros

cycleProjectIndex ::
  MonadDeepState s Env m =>
  (Int -> Int) ->
  m ()
cycleProjectIndex f = do
  pros <- allProjects
  let trans a = f a `rem` length pros
  modifyL @Env Env.currentProjectIndex trans

selectProject ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  Int ->
  m ()
selectProject index = do
  setProjectIndex index
  activateCurrentProject

proPrev ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  m ()
proPrev = do
  cycleProjectIndex (subtract 1)
  activateCurrentProject

proNext ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  m ()
proNext = do
  cycleProjectIndex (+1)
  activateCurrentProject
