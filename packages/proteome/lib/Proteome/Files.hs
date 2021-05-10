module Proteome.Files where

import Control.Lens (_1, view)
import Control.Monad (foldM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Composition ((.:))
import Data.List.Extra (dropEnd)
import qualified Data.List.NonEmpty as NonEmpty (toList, zip)
import Data.List.NonEmpty.Extra (maximumOn1)
import qualified Data.Map as Map (fromList)
import qualified Data.Sequences as Sequences (filterM)
import qualified Data.Text as Text
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
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, PromptFlag(StartInsert, OnlyInsert))
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, markedMenuItems)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimGetOption)
import Text.RE.PCRE.Text (RE, compileRegex)

import Proteome.Data.FilesConfig (FilesConfig(FilesConfig))
import Proteome.Data.FilesError (FilesError)
import qualified Proteome.Data.FilesError as FilesError (FilesError(..))
import Proteome.Files.Source (files)
import Proteome.Files.Syntax (filesSyntax)
import qualified Proteome.Settings as Settings

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
      fmap (view MenuItem.meta) <$> markedMenuItems menu

matchingDirs ::
  MonadIO m =>
  [Path Abs Dir] ->
  Path Rel Dir ->
  m [Path Abs Dir]
matchingDirs bases path =
  Sequences.filterM doesDirExist ((</> path) <$> bases)

dirsWithPrefix ::
  MonadIO m =>
  Text ->
  Path Abs Dir ->
  m [Path Rel Dir]
dirsWithPrefix (Text.toLower -> prefix) dir =
  filter (Text.isPrefixOf prefix . Text.toLower . toText . toFilePath) . fst <$> listDirRel dir

-- |Search all dirs in @bases@ for relative paths starting with @text@.
-- First, split the last path segment (after /) off and collect the subdirectories of @bases@ that start with the
-- remainder. If there is no / in the text, parsing the remainder fails with 'Nothing' and the @bases@ themselves are
-- used.
-- Then, search the resulting dirs for subdirs starting with the last segment.
-- Return the remainder and the relative subdir paths.
matchingPaths ::
  MonadIO m =>
  [Path Abs Dir] ->
  Text ->
  m (Text, [Path Rel Dir])
matchingPaths bases text' =
  (subpath,) . join <$> (traverse (dirsWithPrefix prefix) =<< dirs)
  where
    subpath =
      maybe "" (toText . toFilePath) dir
    dirs =
      maybe (return bases) (matchingDirs bases) dir
    (dir, prefix) =
      first (parseRelDir . toString) $ Text.breakOnEnd "/" text'

commonPrefix :: [Text] -> Maybe Text
commonPrefix (h : t) =
  foldM (fmap (view _1) .: Text.commonPrefixes) h t
commonPrefix a =
  listToMaybe a

tab ::
  MonadIO m =>
  [Path Abs Dir] ->
  Menu (Path Abs File) ->
  Prompt ->
  m (MenuConsumerAction m (), Menu (Path Abs File))
tab bases menu (Prompt _ state' text') = do
  existingBases <- Sequences.filterM doesDirExist bases
  (subpath, paths) <- matchingPaths existingBases text'
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

existingSubdirCount ::
  MonadIO m =>
  [Text] ->
  Path Abs Dir ->
  m Int
existingSubdirCount =
  loop 0
  where
    loop count [] _ =
      pure count
    loop count (h : t) dir =
      case parseRelDir (toString h) of
        Right f ->
          ifM (doesDirExist sub) (loop (count + 1) t sub) (pure count)
          where sub = dir </> f
        Left _ ->
          pure count

createFile ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  NonEmpty (Path Abs Dir) ->
  Menu (Path Abs File) ->
  Prompt ->
  m (MenuConsumerAction m (), Menu (Path Abs File))
createFile bases menu (Prompt _ _ text') = do
  subdirCounts <- traverse (existingSubdirCount dirSegments) bases
  menuQuitWith (maybe err createAndEditFile (parse subdirCounts)) menu
  where
    parse counts =
      (base counts </>) <$> parseRelFile (toString text')
    base counts =
      fst $ maximumOn1 snd (NonEmpty.zip bases counts)
    dirSegments =
      dropEnd 1 $ Text.splitOn "/" text'
    err =
      throwHoist (FilesError.InvalidFilePath text')

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
    ("tab", tab (NonEmpty.toList bases)),
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
readRE name text' =
  maybe (throwHoist (FilesError.BadRE name text')) pure (compileRegex (toString text'))

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
  FilesConfig <$> useRg <*> hidden <*> fs <*> dirs <*> wildignore
  where
    useRg =
      setting Settings.filesUseRg
    hidden =
      setting Settings.filesExcludeHidden
    fs =
      readREs Settings.filesExcludeFiles
    dirs =
      readREs Settings.filesExcludeDirectories
    wildignore =
      Text.splitOn "," <$> (vimGetOption "wildignore")

filesWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  PromptConfig m ->
  Path Abs Dir ->
  [Text] ->
  m ()
filesWith promptConfig cwd paths = do
  conf <- filesConfig
  void $ nvimMenu scratchOptions (files conf nePaths) handler promptConfig Nothing
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
  MonadThrow m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  [Text] ->
  m ()
proFiles paths = do
  cwd <- hoistEitherAs FilesError.BadCwd =<< parseAbsDir <$> nvimCwd
  filesWith (defaultPrompt [StartInsert, OnlyInsert]) cwd paths
