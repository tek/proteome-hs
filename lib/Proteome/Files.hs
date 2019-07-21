module Proteome.Files where

import Control.Lens (_1, view)
import Control.Monad (foldM)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Composition ((.:))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (breakOnEnd, commonPrefixes, isPrefixOf, length, take)
import Path (Abs, Dir, File, Path, Rel, parent, parseAbsDir, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesDirExist, listDirRel)
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.Setting (Setting(Setting))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu.Action (menuContinue, menuQuitWith, menuUpdatePrompt)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, markedMenuItems)
import Ribosome.Msgpack.Error (DecodeError)
import Text.RE.PCRE.Text (RE, compileRegex)

import Proteome.Data.Env (Env)
import Proteome.Data.FilesConfig (FilesConfig(FilesConfig))
import Proteome.Data.FilesError (FilesError)
import qualified Proteome.Data.FilesError as FilesError (FilesError(..))
import Proteome.Files.Source (files)
import Proteome.Files.Syntax (filesSyntax)
import qualified Proteome.Settings as Settings (filesExcludeDirectories, filesExcludeFiles, filesExcludeHidden)

editFile ::
  NvimE e m =>
  Menu (Path Abs File) ->
  Prompt ->
  m (MenuConsumerAction m (), Menu (Path Abs File))
editFile menu _ =
  action menu
  where
    action =
      maybe menuContinue quit marked
    quit =
      menuQuitWith . traverse_ (edit . toFilePath)
    marked =
      view MenuItem.meta <$$> markedMenuItems menu

matchingDirs ::
  MonadIO m =>
  NonEmpty (Path Abs Dir) ->
  Path Rel Dir ->
  m [Path Abs Dir]
matchingDirs bases path =
  filterM doesDirExist ((</> path) <$> NonEmpty.toList bases)

dirsWithPrefix ::
  MonadIO m =>
  Text ->
  Path Abs Dir ->
  m [Path Rel Dir]
dirsWithPrefix prefix dir =
  filter (Text.isPrefixOf prefix . toText . toFilePath) . fst <$> listDirRel dir

-- |Search all dirs in @bases@ for relative paths starting with @text@.
-- First, split the last path segment (after /) off and collect the subdirectories of @bases@ that start with the
-- remainder. If there is no / in the text, parsing the remainder fails with 'Nothing' and the @bases@ themselves are
-- used.
-- Then, search the resulting dirs for subdirs starting with the last segment.
-- Return the remainder and the relative subdir paths.
matchingPaths ::
  MonadIO m =>
  NonEmpty (Path Abs Dir) ->
  Text ->
  m (Text, [Path Rel Dir])
matchingPaths bases text =
  (subpath,) . join <$> (traverse (dirsWithPrefix prefix) =<< dirs)
  where
    subpath =
      maybe "" (toText . toFilePath) dir
    dirs =
      maybe (return $ NonEmpty.toList bases) (matchingDirs bases) dir
    (dir, prefix) =
      first (parseRelDir . toString) $ Text.breakOnEnd "/" text

commonPrefix :: [Text] -> Maybe Text
commonPrefix (h : t) =
  foldM (fmap (view _1) .: Text.commonPrefixes) h t
commonPrefix a =
  listToMaybe a

tab ::
  MonadIO m =>
  NonEmpty (Path Abs Dir) ->
  Menu (Path Abs File) ->
  Prompt ->
  m (MenuConsumerAction m (), Menu (Path Abs File))
tab bases menu (Prompt _ state' text) = do
  (subpath, paths) <- matchingPaths bases text
  action subpath (commonPrefix (toText . toFilePath <$> paths)) menu
  where
    action subpath =
      maybe menuContinue (update . (subpath <>))
    update prefix =
      menuUpdatePrompt (Prompt (Text.length prefix) state' prefix)

createAndEditFile ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  Path Abs File ->
  m ()
createAndEditFile path =
  tryHoistAnyAs err create *>
  edit (toFilePath path)
  where
    err =
      FilesError.CouldntCreateDir (toText (toFilePath dir))
    create =
      createDirIfMissing True dir
    dir =
      parent path

createFile ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  NonEmpty (Path Abs Dir) ->
  Menu (Path Abs File) ->
  Prompt ->
  m (MenuConsumerAction m (), Menu (Path Abs File))
createFile (base :| _) menu (Prompt _ _ text) =
  menuQuitWith (maybe err createAndEditFile parse) menu
  where
    parse =
      (base </>) <$> parseRelFile (toString text)
    err =
      throwHoist (FilesError.InvalidFilePath text)

actions ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  NonEmpty (Path Abs Dir) ->
  [(Text, Menu (Path Abs File) -> Prompt -> m (MenuConsumerAction m (), Menu (Path Abs File)))]
actions bases =
  [
    ("cr", editFile),
    ("tab", tab bases),
    ("c-y", createFile bases)
    ]

parsePath :: Path Abs Dir -> Text -> Maybe (Path Abs Dir)
parsePath _ path | Text.take 1 path == "/" =
  parseAbsDir (toString path)
parsePath cwd path =
  (cwd </>) <$> parseRelDir (toString path)

readRE ::
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  Text ->
  Text ->
  m RE
readRE name text =
  tryHoistAnyAs (FilesError.BadRE name text) (compileRegex (toString text))

readREs ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  MonadDeepError e SettingError m =>
  Setting [Text] ->
  m [RE]
readREs s@(Setting name _ _) =
  traverse (readRE name) =<< setting s

filesConfig ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  m FilesConfig
filesConfig =
  FilesConfig <$> hidden <*> fs <*> dirs
  where
    hidden =
      setting Settings.filesExcludeHidden
    fs =
      readREs Settings.filesExcludeFiles
    dirs =
      readREs Settings.filesExcludeDirectories

filesWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  PromptConfig m ->
  Path Abs Dir ->
  [Text] ->
  m ()
filesWith promptConfig cwd paths = do
  conf <- filesConfig
  void $ nvimMenu scratchOptions (files conf cwd nePaths) handler promptConfig Nothing
  where
    nePaths =
      fromMaybe (cwd :| []) $ nonEmpty absPaths
    absPaths =
      mapMaybe (parsePath cwd) paths
    scratchOptions =
      scratchSyntax [filesSyntax] . defaultScratchOptions $ "proteome-files"
    handler =
      defaultMenu (Map.fromList (actions nePaths))

proFiles ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  [Text] ->
  m ()
proFiles paths = do
  cwd <- hoistEitherAs FilesError.BadCwd =<< parseAbsDir <$> nvimCwd
  filesWith (defaultPrompt True) cwd paths
