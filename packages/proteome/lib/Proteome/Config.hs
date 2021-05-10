module Proteome.Config where

import qualified Control.Lens as Lens (each, toListOf)
import qualified Data.Map as Map (fromList)
import Ribosome.Api.Option (optionString)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (configLog, mainProject)
import Proteome.Data.Project (Project(Project))
import qualified Proteome.Data.Project as Project (types)
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import qualified Proteome.Data.ProjectType as ProjectType (tpe)

globRtp ::
  NvimE e m =>
  Text ->
  m [Text]
globRtp path = do
  rtp <- optionString "runtimepath"
  vimCallFunction "globpath" [toMsgpack rtp, toMsgpack path, toMsgpack False, toMsgpack True]

runtime ::
  NvimE e m =>
  Text ->
  m [Text]
runtime path = do
  vimCommand $ "runtime! " <> toText fpath
  globRtp fpath
  where
    fpath = path <> ".vim"

runtimeConf ::
  NvimE e m =>
  Text ->
  Text ->
  m [Text]
runtimeConf confDir path =
  runtime (confDir <> "/" <> path)

typeProjectConf ::
  NvimE e m =>
  Text ->
  ProjectName ->
  ProjectType ->
  m [Text]
typeProjectConf confDir (ProjectName name') (ProjectType tpe') = do
  tpePaths <- runtimeConf confDir tpe'
  namePaths <- runtimeConf confDir $ tpe' <> "/" <> name'
  return $ tpePaths <> namePaths

readConfigMeta ::
  NvimE e m =>
  Text ->
  Project ->
  m [Text]
readConfigMeta confDir (Project (DirProject name' _ tpe') _ _ _) = do
  paths <- traverse (typeProjectConf confDir name') tpe'
  return $ fromMaybe [] paths
readConfigMeta _ _ = return []

readConfigProject ::
  NvimE e m =>
  Text ->
  Project ->
  m [Text]
readConfigProject confDir project = do
  paths <- traverse (runtimeConf confDir) $ Lens.toListOf (Project.types . Lens.each . ProjectType.tpe) project
  metaPaths <- readConfigMeta confDir project
  return $ join paths <> metaPaths

readConfig ::
  NvimE e m =>
  Text ->
  Project ->
  m [Text]
readConfig confDir project = do
  allPaths <- runtimeConf confDir "all/*"
  projectPaths <- readConfigProject confDir project
  return $ allPaths <> projectPaths

logConfig ::
  MonadDeepState s Env m =>
  [Text] ->
  m ()
logConfig paths =
  modifyL @Env Env.configLog (paths <>)

proReadConfig ::
  NvimE e m =>
  MonadDeepState s Env m =>
  m ()
proReadConfig = do
  main <- getL @Env Env.mainProject
  configs <- readConfig "project" main
  logConfig configs
  afterConfigs <- readConfig "project_after" main
  logConfig afterConfigs

defaultTypeMarkers :: Map ProjectType [Text]
defaultTypeMarkers =
  Map.fromList [
    (ProjectType "haskell", ["stack.yaml", "*.cabal", "cabal.project"]),
    (ProjectType "scala", ["*.sbt"])
    ]
