module Proteome.Filename where

import qualified Chronos
import Control.Monad (foldM)
import Control.Monad.Catch (MonadThrow)
import Data.MessagePack (Object)
import qualified Data.Text as Text
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  addExtension,
  filename,
  parent,
  parseAbsDir,
  reldir,
  splitExtension,
  toFilePath,
  (</>),
  )
import Path.IO (copyFile, doesDirExist, doesFileExist, ensureDir, removeFile)
import Ribosome.Api.Buffer (currentBufferName, edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.IO (bufferGetNumber, bufferSetName, vimCallFunction, vimCommand, vimGetCurrentBuffer)
import Ribosome.Persist (persistencePath)

import Proteome.Data.Env (Proteome)
import qualified Proteome.Data.FilenameError as FilenameError
import Proteome.Data.FilenameError (FilenameError)
import Proteome.Path (
  absoluteParse,
  absoluteParseDir,
  parseAbsDirMaybe,
  parseAbsFileMaybe,
  parseRelDirMaybe,
  parseRelFileMaybe,
  pathText,
  )

data Modification =
  Filename (Path Rel File) Int
  |
  Dir (Path Abs Dir)
  |
  File (Path Abs File)
  deriving (Eq, Show)

dotsInPath :: Text -> Int
dotsInPath path =
  Text.length (Text.filter (== '.') path)

absoluteDir :: Text -> Maybe Modification
absoluteDir =
  fmap Dir . parseAbsDirMaybe

absoluteFile :: Text -> Maybe Modification
absoluteFile =
  fmap File . parseAbsFileMaybe

relativeDir :: Path Abs Dir -> Text -> Maybe Modification
relativeDir cwd spec = do
  rel <- parseRelDirMaybe spec
  pure (Dir (cwd </> rel))

relativeFile :: Path Abs Dir -> Text -> Maybe Modification
relativeFile cwd spec = do
  rel <- parseRelFileMaybe spec
  pure (File (cwd </> rel))

nameOnly :: Text -> Maybe Modification
nameOnly spec = do
  rel <- parseRelFileMaybe spec
  Just (Filename rel (dotsInPath spec))

maybeDir ::
  MonadIO m =>
  Path Abs Dir ->
  Text ->
  m Bool
maybeDir cwd spec =
  fromMaybe False <$> traverse doesDirExist (absoluteParseDir cwd spec)

modification ::
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Path Abs Dir ->
  Text ->
  m Modification
modification cwd (Text.strip -> spec) = do
  existingDir <- maybeDir cwd spec
  hoistMaybe (FilenameError.InvalidPathSpec spec) (cons existingDir spec)
  where
    cons existingDir =
      if name then nameOnly else
      if absolute
      then if explicitDir || existingDir then absoluteDir else absoluteFile
      else if explicitDir then (relativeDir cwd) else (relativeFile cwd)
    name =
      not (Text.any ('/' ==) spec)
    absolute =
      Text.take 1 spec == "/"
    explicitDir =
      Text.takeEnd 1 spec == "/"

checkBufferPath ::
  NvimE e m =>
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Path Abs Dir ->
  m (Path Abs File)
checkBufferPath cwd = do
  name <- currentBufferName
  path <- hoistMaybe FilenameError.BufferPathInvalid (absoluteParse cwd name)
  ifM (doesFileExist path) (pure path) (throwHoist FilenameError.BufferPathInvalid)

extensions :: Int -> Path Rel File -> [String]
extensions 0 _ =
  []
extensions num path =
  case splitExtension path of
    Just (prefix, ext) -> ext : extensions (num - 1) prefix
    Nothing -> []

renameInplace ::
  MonadDeepError e FilenameError m =>
  Path Abs File ->
  Path Rel File ->
  Int ->
  m (Path Abs File)
renameInplace bufPath newName dots = do
  withExt <- hoistMaybe FilenameError.BufferPathInvalid (foldM (flip addExtension) newName extraExtensions)
  pure ((parent bufPath) </> withExt)
  where
    extraExtensions =
      extensions diffDots (filename bufPath)
    diffDots =
      bufDots - dots
    bufDots =
      dotsInPath (pathText bufPath)

assemblePath ::
  MonadDeepError e FilenameError m =>
  Path Abs File ->
  Modification ->
  m (Path Abs File)
assemblePath bufPath = \case
  Filename newName dots ->
    renameInplace bufPath newName dots
  Dir dir ->
    pure (dir </> (filename bufPath))
  File file ->
    pure file

ensureDestinationEmpty ::
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Path Abs File ->
  m ()
ensureDestinationEmpty path =
  whenM (doesFileExist path) (throwHoist (FilenameError.Exists (pathText path)))

prepareDestination ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Path Abs File ->
  m ()
prepareDestination path =
  ifM exists (ensureDestinationEmpty path) create
  where
    exists =
      doesDirExist dir
    create =
      tryHoistAnyAs (FilenameError.CreateDir (pathText dir)) (ensureDir dir)
    dir =
      parent path

getCwd ::
  NvimE e m =>
  MonadDeepError e FilenameError m =>
  m (Path Abs Dir)
getCwd =
  hoistEitherAs FilenameError.BadCwd =<< parseAbsDir <$> nvimCwd

smartModification ::
  NvimE e m =>
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Text ->
  m Modification
smartModification spec = do
  cwd <- getCwd
  modification cwd spec

trashModification ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilenameError m =>
  m Modification
trashModification = do
  cwd <- getCwd
  bufPath <- checkBufferPath cwd
  let original = pathText (filename bufPath)
  Chronos.Time stamp <- liftIO Chronos.now
  trashFile <- hoistMaybe FilenameError.BufferPathInvalid (parseRelFileMaybe [text|#{stamp}_#{original}|])
  trashPath <- persistencePath ([reldir|trash|] </> trashFile)
  let trashDir = parent trashPath
  tryHoistAnyAs (FilenameError.CreateDir (pathText trashDir)) (ensureDir trashDir)
  pure (File trashPath)

pathsForMod ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Modification ->
  m (Path Abs File, Path Abs File)
pathsForMod mod' = do
  cwd <- getCwd
  bufPath <- checkBufferPath cwd
  path <- assemblePath bufPath mod'
  prepareDestination path
  pure (bufPath, path)

updateBuffer ::
  NvimE e m =>
  Path Abs File ->
  m ()
updateBuffer path = do
  buf <- vimGetCurrentBuffer
  bufferSetName buf (pathText path)
  vimCommand "silent write!"

relocate ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Text ->
  Modification ->
  (Path Abs File -> Path Abs File -> m ()) ->
  m ()
relocate action mod' run = do
  (bufPath, destPath) <- pathsForMod mod'
  tryHoistAny (FilenameError.ActionFailed action . show) (run bufPath destPath)

moveFile ::
  MonadIO m =>
  Path Abs File ->
  Path Abs File ->
  m ()
moveFile src dest = do
  copyFile src dest
  removeFile src

move ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Modification ->
  m ()
move mod' = do
  relocate "move" mod' \ buf dest -> do
    moveFile buf dest
    updateBuffer dest

copy ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Modification ->
  m ()
copy mod' =
  relocate "copy" mod' \ src dest -> do
    copyFile src dest
    view :: Object <- vimCallFunction "winsaveview" []
    edit (toFilePath dest)
    vimCallFunction "winrestview" [view]

proMove ::
  Text ->
  Proteome ()
proMove =
  move <=< smartModification

proCopy ::
  Text ->
  Proteome ()
proCopy =
  copy <=< smartModification

proRemove ::
  Proteome ()
proRemove = do
  move =<< trashModification
  buf <- vimGetCurrentBuffer
  ignoreError @RpcError do
    number <- bufferGetNumber buf
    vimCommand ("silent! noautocmd bwipeout! " <> show number)
