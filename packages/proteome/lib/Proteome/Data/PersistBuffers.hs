module Proteome.Data.PersistBuffers where

import Data.Aeson (FromJSON, ToJSON(toEncoding), defaultOptions, genericToEncoding)
import Path (Abs, File, Path)

data PersistBuffers =
  PersistBuffers {
    current :: Maybe (Path Abs File),
    buffers :: [Path Abs File]
  }
  deriving (Eq, Generic, Show, FromJSON)

instance ToJSON PersistBuffers where
  toEncoding = genericToEncoding defaultOptions
