module Proteome.PersistBuffers where

import Control.Monad.Catch (MonadThrow)
import Data.Composition ((.:))
import qualified Data.Text as Text (null)
import Path (Abs, Dir, Path, Rel, parseRelDir, relfile, toFilePath, (</>))
import Ribosome.Api.Buffer (bufferForFile, buflisted, edit)
import Ribosome.Control.Lock (lockOrSkip)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (bufferGetName, vimCommand, vimGetCurrentBuffer)
import Ribosome.Persist (mayPersistLoad, persistStore)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (buffers, mainProject)
import Proteome.Data.PersistBuffers (PersistBuffers(PersistBuffers))
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Path (existingFile)

projectPaths ::
  MonadThrow m =>
  MonadDeepState s Env m =>
  m (Maybe (Path Abs Dir, Path Rel Dir))
projectPaths =
  examine =<< getL @Env Env.mainProject
  where
    examine (Project (DirProject (ProjectName name) (ProjectRoot root) (Just (ProjectType tpe))) _ _ _) =
      Just . (root,) <$> ((</>) <$> parseRelDir (toString tpe) <*> parseRelDir (toString name))
    examine _ =
      return Nothing

unsafeStoreBuffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  Path Abs Dir ->
  Path Rel Dir ->
  m ()
unsafeStoreBuffers cwd path = do
  names <- traverse bufferGetName =<< filterM buflisted =<< getL @Env Env.buffers
  files <- catMaybes <$> traverse (existingFile cwd) names
  persistStore (path </> [relfile|buffers|]) (PersistBuffers (listToMaybe files) files)

safeStoreBuffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  Path Abs Dir ->
  Path Rel Dir ->
  m ()
safeStoreBuffers =
  lockOrSkip "store-buffers" .: unsafeStoreBuffers

storeBuffers ::
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  m ()
storeBuffers =
  traverse_ (uncurry safeStoreBuffers) =<< projectPaths

decodePersistBuffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  Path Rel Dir ->
  m (Maybe PersistBuffers)
decodePersistBuffers path =
  mayPersistLoad (path </> [relfile|buffers|])

restoreBuffers ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  PersistBuffers ->
  m ()
restoreBuffers (PersistBuffers active rest) = do
  traverse_ loadActive active
  traverse_ add rest
  setL @Env Env.buffers . catMaybes =<< traverse (bufferForFile . toText . toFilePath) rest
  where
    add a =
      vimCommand ("silent! badd " <> toText (toFilePath a))
    loadActive path = do
      currentBufferName <- bufferGetName =<< vimGetCurrentBuffer
      when (Text.null currentBufferName) (edit (toFilePath path))

unsafeLoadBuffers ::
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  Path Rel Dir ->
  m ()
unsafeLoadBuffers path = do
  pb <- decodePersistBuffers path
  traverse_ restoreBuffers pb

safeLoadBuffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  Path Rel Dir ->
  m ()
safeLoadBuffers path =
  lockOrSkip "load-buffers" $ unsafeLoadBuffers path

loadBuffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  m ()
loadBuffers =
  traverse_ (safeLoadBuffers . snd) =<< projectPaths
