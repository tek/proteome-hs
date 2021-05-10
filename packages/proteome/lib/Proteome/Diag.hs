module Proteome.Diag where

import qualified Control.Lens as Lens (view)
import Data.Map (foldMapWithKey)
import qualified Data.Text as Text (intercalate)
import Neovim (CommandArguments)
import Path (toFilePath)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.Errors (ComponentName(ComponentName), Error(Error), Errors(Errors))
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchFocus)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (configLog, errors, mainProject, projects)
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectLang (ProjectLang(ProjectLang))
import qualified Proteome.Data.ProjectLang as ProjectLang (lang)
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject, VirtualProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import qualified Proteome.Data.ProjectType as ProjectType (tpe)
import Proteome.Data.TagsError (TagsError)
import Proteome.Tags (tagsCommand)

formatLang :: Maybe ProjectLang -> Text
formatLang (Just (ProjectLang lang)) = lang
formatLang Nothing = "none"

formatType :: Maybe ProjectType -> Text
formatType (Just (ProjectType tpe)) = tpe
formatType Nothing = "none"

formatMeta ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  ProjectMetadata ->
  [ProjectLang] ->
  m [Text]
formatMeta (VirtualProject (ProjectName name)) _ = return ["name: " <> name]
formatMeta (DirProject (ProjectName name) r@(ProjectRoot root) tpe) langs = do
  (tagsCmd, tagsArgs) <- tagsCommand r langs
  return [
    "name: " <> name,
    "root: " <> (toText . toFilePath) root,
    "type: " <> formatType tpe,
    "tags cmd: " <> tagsCmd <> " " <> tagsArgs
    ]

formatMain ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  Project ->
  m [Text]
formatMain (Project meta types lang langs) = do
  metaContent <- formatMeta meta langs
  return $ metaContent <> [
    "types: " <> Text.intercalate ", " (Lens.view ProjectType.tpe <$> types),
    "main language: " <> formatLang lang,
    "languages: " <> Text.intercalate ", " (Lens.view ProjectLang.lang <$> langs)
    ]

formatExtraProjects ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  [Project] ->
  m [Text]
formatExtraProjects projects = do
  formatted <- traverse formatMain projects
  return $ ["", "Extra projects", ""] <> intercalate [""] formatted

formatExtraProjectsIfNonempty ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Env m =>
  m [Text]
formatExtraProjectsIfNonempty = do
  projects <- getL @Env Env.projects
  case projects of
    _ : _ -> formatExtraProjects projects
    _ -> return []

formatError :: Error -> [Text]
formatError (Error stamp (ErrorReport _ (h : message) _)) = (show stamp <> " | " <> h) : message
formatError _ = []

formatComponentErrors :: ComponentName -> [Error] -> [Text]
formatComponentErrors (ComponentName name) errors@(_ : _) =
  [name <> "", ""] <> (errors >>= formatError)
formatComponentErrors _ _ = []

formatErrorLog :: Errors -> [Text]
formatErrorLog (Errors errors) =
  case compErrors of
    _ : _ -> ["", "Errors", ""] <> compErrors
    _ -> []
  where
    compErrors = foldMapWithKey formatComponentErrors errors

diagnostics ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Env m =>
  m [Text]
diagnostics = do
  main <- formatMain =<< getL @Env Env.mainProject
  extra <- formatExtraProjectsIfNonempty
  confLog <- getL @Env Env.configLog
  errors <- getL @Env Env.errors
  return $ header <> main <> extra <> ["", "loaded config files:"] <> confLog <> formatErrorLog errors
  where
    header =
      ["Diagnostics", "", "Main project", ""]

proDiag ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  CommandArguments ->
  m ()
proDiag _ = do
  content <- diagnostics
  void $ showInScratch content (scratchFocus $ defaultScratchOptions "proteome-diagnostics")
