module Proteome.Data.Project where

import Proteome.Data.ProjectLang (ProjectLang(ProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata)
import Proteome.Data.ProjectType (ProjectType(ProjectType))

data Project =
  Project {
    _meta :: ProjectMetadata,
    _types :: [ProjectType],
    _lang :: Maybe ProjectLang,
    _langs :: [ProjectLang]
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)

makeClassy ''Project

instance Default Project where
  def = Project def def def def

langOrType :: Maybe ProjectLang -> Maybe ProjectType -> Maybe ProjectLang
langOrType (Just lang') _ = Just lang'
langOrType Nothing (Just (ProjectType tpe')) = Just (ProjectLang tpe')
langOrType _ _ = Nothing
