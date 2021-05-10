module Proteome.Data.AddOptions where

import Proteome.Data.ProjectName (ProjectName)
import Proteome.Data.ProjectType (ProjectType)

data AddOptions =
  AddOptions {
    name :: ProjectName,
    tpe :: ProjectType,
    activate :: Maybe Bool
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
