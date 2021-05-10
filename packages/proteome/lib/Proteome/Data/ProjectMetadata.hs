module Proteome.Data.ProjectMetadata where

import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot)
import Proteome.Data.ProjectType (ProjectType)

data ProjectMetadata =
  DirProject {
    name :: ProjectName,
    root :: ProjectRoot,
    tpe :: Maybe ProjectType
  }
  | VirtualProject {
    name :: ProjectName
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)

instance Default ProjectMetadata where
  def = VirtualProject (ProjectName "main")
