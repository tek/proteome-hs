module Proteome.Files.Source where

import Conduit (ConduitT, mapC, (.|))
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Monad.Catch (MonadThrow)
import qualified Data.Conduit.Combinators as Conduit
import Data.Conduit.TMChan (TMChan, closeTMChan, newTMChan, sourceTMChan, writeTMChan)
import qualified Data.List.NonEmpty as NonEmpty (toList, zip)
import qualified Data.Set as Set (fromList, toList)
import qualified Data.Text as Text
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  dirname,
  filename,
  isProperPrefixOf,
  parent,
  parseAbsFile,
  relfile,
  stripProperPrefix,
  toFilePath,
  )
import Path.IO (findExecutable, walkDir)
import qualified Path.IO as WalkAction (WalkAction(WalkExclude))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import System.FilePattern ((?==))
import Text.RE.PCRE.Text (RE, anyMatches, (*=~))

import Proteome.Data.FileScanItem (FileScanItem(FileScanItem))
import Proteome.Data.FilesConfig (FilesConfig(FilesConfig))
import Proteome.Grep.Process (grep)
import Proteome.Path (dropSlash)

matchPath :: [RE] -> Path Abs t -> Bool
matchPath exclude path =
  any (anyMatches . match) exclude
  where
    match regex =
      toText (toFilePath path) *=~ regex

hiddenFilter ::
  (Path Abs t -> Path Rel t) ->
  Bool ->
  Path Abs t ->
  Bool
hiddenFilter lastSegment True =
  Text.isPrefixOf "." . toText . toFilePath . lastSegment
hiddenFilter _ False =
  const False

filterFiles ::
  Bool ->
  [RE] ->
  [String] ->
  [Path Abs File] ->
  [Path Abs File]
filterFiles excludeHidden patterns wildignore =
  filter (not . cond)
  where
    cond path =
      matchPath patterns path ||
      hiddenFilter filename excludeHidden path ||
      any (?== name) wildignore
      where
        name =
          toFilePath (filename path)

filterDirs ::
  Bool ->
  [RE] ->
  [Path Abs Dir] ->
  [Path Abs Dir]
filterDirs excludeHidden patterns =
  filter pred'
  where
    pred' a =
      matchPath patterns a || hiddenFilter dirname excludeHidden a

scan ::
  MonadIO m =>
  FilesConfig ->
  TMChan [FileScanItem] ->
  Path Abs Dir ->
  Maybe Text ->
  m ()
scan (FilesConfig _ excludeHidden ignoreFiles ignoreDirs wildignore) chan dir baseIndicator =
  walkDir enqueue dir
  where
    enqueue _ dirs files' =
      atomically (writeTMChan chan (cons <$> filterFiles excludeHidden ignoreFiles (toString <$> wildignore) files')) $>
      WalkAction.WalkExclude (filterDirs excludeHidden ignoreDirs dirs)
    cons =
      FileScanItem dir baseIndicator

rgExists ::
  MonadIO m =>
  m Bool
rgExists =
  isJust <$> findExecutable [relfile|rg|]

runScanners ::
  MonadIO m =>
  MonadBaseControl IO m =>
  FilesConfig ->
  TMChan [FileScanItem] ->
  NonEmpty (Path Abs Dir, Maybe Text) ->
  m (NonEmpty ())
runScanners conf chan paths = do
  threads <- traverse (async . uncurry (scan conf chan)) paths
  traverse wait threads <* atomically (closeTMChan chan)

withBaseIndicators ::
  NonEmpty (Path Abs Dir) ->
  NonEmpty (Path Abs Dir, Maybe Text)
withBaseIndicators bases@(_ :| []) =
  (, Nothing) <$> bases
withBaseIndicators bases =
  NonEmpty.zip bases (findSegment bases)
  where
    findSegment paths =
      if namesUnique paths
      then Just . dropSlash . dirname <$> paths
      else if allEqual next then Nothing <$ paths else findSegment next
      where
        next = parent <$> paths
    namesUnique paths =
      uniq names == NonEmpty.toList names
      where
        names = dirname <$> paths
    allEqual paths =
      length (uniq paths) == 1
    uniq as =
      Set.toList (Set.fromList (NonEmpty.toList as))

formatFileLine ::
  Path Abs Dir ->
  Maybe Text ->
  Path Abs File ->
  Text
formatFileLine base baseIndicator path =
  " * " <> maybe "" indicator baseIndicator <> toText (maybe (toFilePath path) toFilePath relativePath)
  where
    relativePath =
      stripProperPrefix base path
    indicator name =
      "[" <> name <> "] "

filesNative ::
  MonadIO m =>
  MonadBaseControl IO m =>
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  ConduitT () [MenuItem (Path Abs File)] m ()
filesNative conf paths = do
  chan <- atomically newTMChan
  void . lift . async $ runScanners conf chan (withBaseIndicators paths)
  sourceTMChan chan .| mapC (fmap menuItem)
  where
    menuItem (FileScanItem base baseIndicator path) =
      simpleMenuItem path (formatFileLine base baseIndicator path)

rgExcludes :: FilesConfig -> [Text]
rgExcludes (FilesConfig _ _ _ _ wilds) =
  concat (wild <$> wilds)
  where
    wild i =
      ["--glob", [text|!#{i}|]]

findBase ::
  Path Abs File ->
  NonEmpty (Path Abs Dir, Maybe Text) ->
  Maybe (Path Abs Dir, Maybe Text)
findBase file =
  find ((`isProperPrefixOf` file) . fst)

rgMenuItem ::
  NonEmpty (Path Abs Dir, Maybe Text) ->
  Text ->
  Maybe (MenuItem (Path Abs File))
rgMenuItem bases (toString -> pathText) = do
  path <- parseAbsFile pathText
  (base, baseIndicator) <- findBase path bases
  pure (simpleMenuItem path (formatFileLine base baseIndicator path))

filesRg ::
  MonadIO m =>
  MonadThrow m =>
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  ConduitT () [MenuItem (Path Abs File)] m ()
filesRg conf paths =
  grep "rg" ("--files" : excludes <> patterns) .| mapC item .| mapC maybeToList .| Conduit.chunksOfE 100
  where
    patterns =
      toText . toFilePath <$> toList paths
    excludes =
      rgExcludes conf
    item =
      rgMenuItem (withBaseIndicators paths)

files ::
  MonadIO m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  ConduitT () [MenuItem (Path Abs File)] m ()
files conf@(FilesConfig useRg _ _ _ _) paths =
  ifM ((useRg &&) <$> rgExists) (filesRg conf paths) (filesNative conf paths)
