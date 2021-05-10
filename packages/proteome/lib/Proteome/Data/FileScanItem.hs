module Proteome.Data.FileScanItem where

import Path (Abs, Dir, File, Path)

data FileScanItem =
  FileScanItem {
    _base :: Path Abs Dir,
    _baseIndicator :: Maybe Text,
    _path :: Path Abs File
  }
  deriving (Eq, Show)
