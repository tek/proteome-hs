module Proteome.Tags where

import Control.Concurrent.Lifted (fork)
import qualified Control.Lens as Lens (view)
import Data.Composition ((.:))
import qualified Data.Text as Text (intercalate, replace)
import GHC.IO.Exception (ExitCode(..))
import Path (File, Path, Rel, addExtension, toFilePath, (</>))
import Path.IO (doesFileExist, removeFile, renameFile)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Exception (catchAnyAs)
import Ribosome.Control.Lock (lockOrSkip)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report (processErrorReport)
import System.Log (Priority(NOTICE))
import System.Process.Typed (readProcess, setWorkingDir, shell)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (mainProject, projects)
import Proteome.Data.Project (Project (Project), langOrType)
import Proteome.Data.ProjectLang (ProjectLang())
import qualified Proteome.Data.ProjectLang as ProjectLang (lang)
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.TagsError (TagsError)
import qualified Proteome.Data.TagsError as TagsError (TagsError(Path))
import qualified Proteome.Settings as Settings (tagsArgs, tagsCommand, tagsFileName, tagsFork)

replaceFormatItem :: Text -> (Text, Text) -> Text
replaceFormatItem original (placeholder, replacement) =
  Text.replace ("{" <> placeholder <> "}") replacement original

formatTagsArgs ::
  [ProjectLang] ->
  ProjectRoot ->
  Path Rel File ->
  Text ->
  Text
formatTagsArgs langs (ProjectRoot root) fileName formatString =
  foldl' @[] replaceFormatItem formatString formats
  where
    formats = [
      ("langsComma", Text.intercalate "," $ fmap (Lens.view ProjectLang.lang) langs),
      ("tagFile", toText . toFilePath $ root </> fileName),
      ("root", toText . toFilePath $ root)
      ]

tempname ::
  MonadDeepError e TagsError m =>
  Path Rel File ->
  m (Path Rel File)
tempname name =
  hoistEitherAs err (addExtension ".tmp" name)
  where
    err =
      TagsError.Path "appending tempname suffix"

deleteTags ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  ProjectRoot ->
  m ()
deleteTags (ProjectRoot root) = do
  name <- setting Settings.tagsFileName
  path <- (root </>) <$> tempname name
  exists <- doesFileExist path
  when exists $ catchAnyAs () (removeFile path)

replaceTags ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  ProjectRoot ->
  m ()
replaceTags (ProjectRoot root) = do
  name <- setting Settings.tagsFileName
  temppath <- (root </>) <$> tempname name
  catchAnyAs () $ renameFile temppath (root </> name)

notifyError ::
  NvimE e m =>
  MonadRibo m =>
  Text ->
  m ()
notifyError e =
  processErrorReport "tags" $ ErrorReport "tag generation failed" ["tag subprocess failed: " <> e] NOTICE

tagsProcess ::
  ProjectRoot ->
  Text ->
  Text ->
  IO (ExitCode, Text)
tagsProcess (ProjectRoot root) cmd args = do
  (exitCode, _, err) <- readProcess (conf prc)
  return (exitCode, decodeUtf8 err)
  where
    prc =
      shell $ "unset STACK_IN_NIX_SHELL; " <> toString cmd <> " " <> toString args
    conf =
      setWorkingDir (toFilePath root)

executeTags ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  ProjectRoot ->
  Text ->
  Text ->
  m ()
executeTags projectRoot@(ProjectRoot root) cmd args = do
  deleteTags projectRoot
  logDebug $ "executing tags: `" <> cmd <> " " <> args <> "` in directory " <> show root
  (exitcode, errorOutput) <- liftIO $ tagsProcess projectRoot cmd args
  case exitcode of
    ExitSuccess -> replaceTags projectRoot
    ExitFailure _ -> notifyError errorOutput

tagsCommand ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  ProjectRoot ->
  [ProjectLang] ->
  m (Text, Text)
tagsCommand root langs = do
  cmd <- setting Settings.tagsCommand
  args <- setting Settings.tagsArgs
  fileName <- setting Settings.tagsFileName
  tmp <- tempname fileName
  return (cmd, formatTagsArgs langs root tmp args)

regenerateTags ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  ProjectRoot ->
  [ProjectLang] ->
  m ()
regenerateTags root langs = do
  (cmd, args) <- tagsCommand root langs
  executeTags root cmd args

projectTags ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  Project ->
  m ()
projectTags (Project (DirProject _ root tpe) _ lang langs) =
  regenerateTags root (maybeToList (langOrType lang tpe) <> langs)
projectTags _ = return ()

proTags ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  m ()
proTags = do
  main <- getL @Env Env.mainProject
  extra <- getL @Env Env.projects
  wantFork <- setting Settings.tagsFork
  runner wantFork main extra
  where
    runner wantFork =
      if wantFork then void . fork .: run else run
    run main extra =
      lockOrSkip "tags" $ traverse_ projectTags (main : extra)
