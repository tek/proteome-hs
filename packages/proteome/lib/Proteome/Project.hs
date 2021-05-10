module Proteome.Project where

import qualified Control.Lens as Lens (element, firstOf)
import Path (Abs, Dir, Path, dirname, parent)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (currentProjectIndex, mainProject, projects)
import Proteome.Data.Project (Project)
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Path (dropSlash)

allProjects ::
  MonadDeepState s Env m =>
  m [Project]
allProjects = do
  main <- getL @Env Env.mainProject
  extra <- getL @Env Env.projects
  return $ main : extra

currentProject ::
  MonadDeepState s Env m =>
  m (Maybe Project)
currentProject = do
  index <- getL @Env Env.currentProjectIndex
  Lens.firstOf (Lens.element index) <$> allProjects

pathData :: Path Abs Dir -> (ProjectRoot, ProjectName, ProjectType)
pathData root =
  (
    ProjectRoot root,
    ProjectName . dropSlash . dirname $ root,
    ProjectType . dropSlash . dirname . parent $ root
    )
