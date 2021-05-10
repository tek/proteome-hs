module Proteome.Data.ActiveProject where

import Proteome.Data.ProjectLang (ProjectLang)
import Proteome.Data.ProjectName (ProjectName)
import Proteome.Data.ProjectType (ProjectType)

data ActiveProject =
  ActiveProject {
    name :: ProjectName,
    tpe :: ProjectType,
    lang :: Maybe ProjectLang
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
