module Proteome.Project.Activate(
  activateProject,
  activateCurrentProject,
  proPrev,
  proNext,
) where

import Control.Lens (over)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist)
import Neovim (vim_command')
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Proteome.Data.Project (
  Project(Project, meta),
  ProjectMetadata(DirProject, VirtualProject),
  ProjectType(ProjectType),
  ProjectRoot(ProjectRoot),
  )
import Proteome.Data.ActiveProject (ActiveProject(ActiveProject))
import Proteome.Data.Proteome (Proteome)
import Proteome.Project (currentProject, allProjects)
import qualified Proteome.Data.Env as Env (_currentProjectIndex)
import qualified Proteome.Settings as S (active)

activeProject :: Project -> ActiveProject
activeProject (Project (DirProject name _ tpe) _ _ _) = ActiveProject name (fromMaybe (ProjectType "none") tpe)
activeProject (Project (VirtualProject name) _ _ _) = ActiveProject name (ProjectType "virtual")

activateDirProject :: ProjectMetadata -> Ribo e ()
activateDirProject (DirProject _ (ProjectRoot root) _) = do
  exists <- liftIO $ doesDirectoryExist root
  when exists $ vim_command' $ "chdir " ++ root
activateDirProject _ = return ()

activateProject :: Project -> Ribo e ()
activateProject project = do
  updateSetting S.active $ activeProject project
  activateDirProject (meta project)

activateCurrentProject :: Proteome ()
activateCurrentProject = do
  pro <- currentProject
  mapM_ activateProject pro

cycleProjectIndex :: (Int -> Int) -> Proteome ()
cycleProjectIndex f = do
  pros <- allProjects
  let
    trans:: Int -> Int
    trans a = f a `rem` length pros
  Ribo.modify $ over Env._currentProjectIndex trans

proPrev :: Proteome ()
proPrev = do
  cycleProjectIndex (subtract 1)
  activateCurrentProject

proNext :: Proteome ()
proNext = do
  cycleProjectIndex (+1)
  activateCurrentProject