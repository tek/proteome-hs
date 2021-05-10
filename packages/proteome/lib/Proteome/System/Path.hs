module Proteome.System.Path where

import Path (Abs, File, Path, parseRelFile)
import Path.IO (findExecutable)

import Proteome.Data.GrepError (GrepError)
import qualified Proteome.Data.GrepError as GrepError (GrepError(NoSuchExecutable, NotInPath))

findExe ::
  MonadIO m =>
  MonadDeepError e GrepError m =>
  Text ->
  m (Path Abs File)
findExe exe = do
  path <- hoistEitherAs parseError $ parseRelFile (toString exe)
  hoistMaybe notInPath =<< findExecutable path
  where
    parseError =
      GrepError.NoSuchExecutable exe
    notInPath =
      GrepError.NotInPath exe
