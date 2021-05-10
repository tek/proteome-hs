module Proteome.Path where

import qualified Data.Text as Text
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  parseAbsDir,
  parseAbsFile,
  parseRelDir,
  parseRelFile,
  toFilePath,
  (</>),
  )
import Path.IO (doesFileExist)

parsePathMaybe ::
  (FilePath -> Either a (Path b t)) ->
  Text ->
  Maybe (Path b t)
parsePathMaybe parser =
  rightToMaybe . parser . toString

parseAbsDirMaybe ::
  Text ->
  Maybe (Path Abs Dir)
parseAbsDirMaybe =
  parsePathMaybe parseAbsDir

parseAbsFileMaybe ::
  Text ->
  Maybe (Path Abs File)
parseAbsFileMaybe =
  parsePathMaybe parseAbsFile

parseRelDirMaybe ::
  Text ->
  Maybe (Path Rel Dir)
parseRelDirMaybe =
  parsePathMaybe parseRelDir

parseRelFileMaybe ::
  Text ->
  Maybe (Path Rel File)
parseRelFileMaybe =
  parsePathMaybe parseRelFile

absoluteParseDir ::
  Path Abs Dir ->
  Text ->
  Maybe (Path Abs Dir)
absoluteParseDir cwd spec =
  tryAbsolute <|> tryRelative
  where
    specS =
      toString spec
    tryAbsolute =
      rightToMaybe (parseAbsDir specS)
    tryRelative =
      makeAbsolute <$> rightToMaybe (parseRelDir specS)
    makeAbsolute path =
      cwd </> path

absoluteParse ::
  Path Abs Dir ->
  Text ->
  Maybe (Path Abs File)
absoluteParse cwd spec =
  tryAbsolute <|> tryRelative
  where
    specS =
      toString spec
    tryAbsolute =
      rightToMaybe (parseAbsFile specS)
    tryRelative =
      makeAbsolute <$> rightToMaybe (parseRelFile specS)
    makeAbsolute path =
      cwd </> path

existingFile ::
  MonadIO m =>
  Path Abs Dir ->
  Text ->
  m (Maybe (Path Abs File))
existingFile cwd spec =
  join <$> traverse check (absoluteParse cwd spec)
  where
    check path = do
      exists <- doesFileExist path
      return $ if exists then Just path else Nothing

pathText :: Path b t -> Text
pathText =
  toText . toFilePath

dropSlash :: Path b t -> Text
dropSlash =
  Text.dropWhileEnd ('/' ==) . pathText
