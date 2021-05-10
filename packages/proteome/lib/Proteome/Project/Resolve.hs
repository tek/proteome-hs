module Proteome.Project.Resolve where

import qualified Control.Lens as Lens (over, view)
import Control.Monad (foldM)
import Control.Monad.Extra (firstJustM)
import Data.List (nub)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (toList, union)
import Path (Abs, Dir, Path, parent, parseRelDir, toFilePath, (</>))
import Path.IO (doesDirExist)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Exception (catchAnyAs)
import Ribosome.Data.Foldable (findMapMaybeM)
import Ribosome.Data.SettingError (SettingError)
import System.FilePath.Glob (globDir1)
import qualified System.FilePath.Glob as Glob (compile)
import System.FilePattern.Directory (getDirectoryFiles)

import Proteome.Config (defaultTypeMarkers)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import qualified Proteome.Data.ProjectConfig as ProjectConfig (baseDirs, typeMarkers)
import Proteome.Data.ProjectLang (ProjectLang(ProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject, VirtualProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectSpec (ProjectSpec(ProjectSpec))
import qualified Proteome.Data.ProjectSpec as PS (ProjectSpec(..))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Data.ResolveError (ResolveError)
import qualified Proteome.Data.ResolveError as ResolveError (ResolveError(..))
import Proteome.Path (parseAbsDirMaybe)
import Proteome.Project (pathData)
import qualified Proteome.Settings as Settings (projectConfig, projects)

projectFromSegments :: ProjectType -> ProjectName -> ProjectRoot -> Project
projectFromSegments tpe name root =
  Project (DirProject name root (Just tpe)) [] Nothing []

projectFromSpec :: ProjectSpec -> Project
projectFromSpec (ProjectSpec name root tpe types lang langs) =
  Project (DirProject name root tpe) types lang langs

hasProjectRoot :: ProjectRoot -> ProjectSpec -> Bool
hasProjectRoot root spec = root == PS.root spec

hasProjectTypeName :: ProjectType -> ProjectName -> ProjectSpec -> Bool
hasProjectTypeName tpe' name' (ProjectSpec name _ (Just tpe) _ _ _) =
  name' == name && tpe' == tpe
hasProjectTypeName _ _ _ = False

byProjectTypeName :: [ProjectSpec] -> ProjectName -> ProjectType -> Maybe ProjectSpec
byProjectTypeName specs name tpe = find (hasProjectTypeName tpe name) specs

matchProjectBases :: [Path Abs Dir] -> ProjectRoot -> Bool
matchProjectBases baseDirs (ProjectRoot root) = (parent . parent) root `elem` baseDirs

byProjectBaseSubpath ::
  MonadIO m =>
  MonadDeepError e ResolveError m =>
  ProjectName ->
  ProjectType ->
  Path Abs Dir ->
  m (Maybe Project)
byProjectBaseSubpath n@(ProjectName name) t@(ProjectType tpe) base = do
  tpePath <- hoistEitherAs (ResolveError.ParsePath tpe) $ parseRelDir (toString tpe)
  namePath <- hoistEitherAs (ResolveError.ParsePath name) $ parseRelDir (toString name)
  let root = base </> tpePath </> namePath
  exists <- doesDirExist root
  return $ if exists then Just $ projectFromSegments t n (ProjectRoot root) else Nothing

byProjectBasesSubpath ::
  MonadIO m =>
  MonadDeepError e ResolveError m =>
  [Path Abs Dir] ->
  ProjectName ->
  ProjectType ->
  m (Maybe Project)
byProjectBasesSubpath baseDirs name tpe =
  foldM subpath Nothing baseDirs
  where
    subpath (Just p) _ = return (Just p)
    subpath Nothing a = byProjectBaseSubpath name tpe a

virtualProject :: ProjectName -> Project
virtualProject name = Project (VirtualProject name) [] Nothing []

resolveByTypeAndPath :: [Path Abs Dir] -> ProjectName -> ProjectType -> ProjectRoot -> Maybe Project
resolveByTypeAndPath baseDirs name tpe root =
  if matchProjectBases baseDirs root then Just (projectFromSegments tpe name root) else Nothing

resolveByType ::
  MonadIO m =>
  MonadDeepError e ResolveError m =>
  [Path Abs Dir] ->
  [ProjectSpec] ->
  Maybe ProjectRoot ->
  ProjectName ->
  ProjectType ->
  m (Maybe Project)
resolveByType baseDirs explicit root name tpe = do
  byBaseSubpath <- byProjectBasesSubpath baseDirs name tpe
  return $ byPath <|> byBaseSubpath <|> fmap projectFromSpec byTypeName
  where
    byTypeName = byProjectTypeName explicit name tpe
    byPath = root >>= resolveByTypeAndPath baseDirs name tpe

fromProjectRoot :: Path Abs Dir -> Project
fromProjectRoot dir =
  projectFromSegments tpe name root
  where
    (root, name, tpe) = pathData dir

projectFromNameIn ::
  ∀ m .
  MonadIO m =>
  ProjectName ->
  Path Abs Dir ->
  m (Maybe Project)
projectFromNameIn (ProjectName name) base =
  fmap fromProjectRoot <$> join . find isJust <$> matches
  where
    matches =
      fmap (parseAbsDirMaybe . toText) <$> glob
    glob =
      liftIO $ globDir1 (Glob.compile ("*/" <> toString name)) (toFilePath base)

resolveByName ::
  MonadIO m =>
  [Path Abs Dir] ->
  ProjectName ->
  m (Maybe Project)
resolveByName baseDirs name =
  findMapMaybeM (projectFromNameIn name) baseDirs

globDir ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  [Text] ->
  m (Maybe FilePath)
globDir root patterns =
  listToMaybe <$> catchAnyAs [] (liftIO $ getDirectoryFiles rootS patternsS)
  where
    patternsS =
      toString <$> patterns
    rootS =
      toFilePath root

resolveFromDirContents ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Map ProjectType [Text] ->
  ProjectName ->
  ProjectRoot ->
  m (Maybe Project)
resolveFromDirContents typeMarkers name projectRoot@(ProjectRoot root) =
  fmap cons <$> firstJustM match (Map.toList typeMarkers)
  where
    cons projectType =
      projectFromSegments projectType name projectRoot
    match (tpe, patterns) =
      (tpe <$) <$> globDir root patterns

resolveByRoot ::
  MonadIO m =>
  MonadBaseControl IO m =>
  ProjectConfig ->
  ProjectName ->
  [ProjectSpec] ->
  ProjectRoot ->
  m (Maybe Project)
resolveByRoot (ProjectConfig _ _ _ _ typeMarkers _ _) name explicit root =
  maybe (resolveFromDirContents typeMarkers name root) (return . Just . projectFromSpec) fromExplicit
  where
    fromExplicit = find (hasProjectRoot root) explicit

augment :: (Eq a, Ord k) => Map k [a] -> k -> [a] -> [a]
augment m tpe as =
  case m !? tpe of
    Just extra -> nub $ as ++ extra
    Nothing -> as

augmentTypes :: ProjectConfig -> ProjectType -> [ProjectType] -> [ProjectType]
augmentTypes (ProjectConfig _ _ _ typeMap _ _ _) =
  augment typeMap

realLang :: ProjectConfig -> ProjectType -> ProjectLang
realLang (ProjectConfig _ _ _ _ _ langMap _) t@(ProjectType tpe) =
  fromMaybe (ProjectLang tpe) (langMap !? t)

augmentLangs :: ProjectConfig -> ProjectLang -> [ProjectLang] -> [ProjectLang]
augmentLangs (ProjectConfig _ _ _ _ _ _ langsMap) =
  augment langsMap

augmentFromConfig :: ProjectConfig -> Project -> Project
augmentFromConfig config (Project meta@(DirProject _ _ (Just tpe)) types lang langs) =
  Project meta (augmentTypes config tpe types) (Just realLang') (augmentLangs config realLang' langs)
  where
    realLang' = fromMaybe (realLang config tpe) lang
augmentFromConfig _ project = project

resolveProject ::
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e ResolveError m =>
  [ProjectSpec] ->
  ProjectConfig ->
  Maybe ProjectRoot ->
  ProjectName ->
  Maybe ProjectType ->
  m Project
resolveProject explicit config root name tpe = do
  byType <- join <$> traverse (resolveByType baseDirs explicit root name) tpe
  byName <- if isJust root then return Nothing else resolveByName baseDirs name
  byRoot <- join <$> traverse (resolveByRoot config name explicit) root
  let byNameOrVirtual = fromMaybe (virtualProject name) byName
  let byTypeOrName = fromMaybe byNameOrVirtual byType
  let project = fromMaybe byTypeOrName byRoot
  logDebug @Text $ logMsg byType byName byRoot
  return $ augmentFromConfig config project
  where
    baseDirs =
      Lens.view ProjectConfig.baseDirs config
    logMsg byType byName byRoot =
      "resolved project: byType(" <> show byType <> ") byName(" <> show byName <> ") byRoot(" <> show byRoot <> ")"

projectConfig ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  m ProjectConfig
projectConfig =
  Lens.over ProjectConfig.typeMarkers (`Map.union` defaultTypeMarkers) <$> setting Settings.projectConfig

resolveProjectFromConfig ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  Maybe ProjectRoot ->
  ProjectName ->
  Maybe ProjectType ->
  m Project
resolveProjectFromConfig root name tpe = do
  explicit <- setting Settings.projects
  config <- projectConfig
  resolveProject explicit config root name tpe
