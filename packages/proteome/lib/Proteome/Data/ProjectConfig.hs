module Proteome.Data.ProjectConfig where

import Path (Abs, Dir, Path)

import Proteome.Data.ProjectLang (ProjectLang)
import Proteome.Data.ProjectType (ProjectType)

data ProjectConfig =
  ProjectConfig {
    _baseDirs :: [Path Abs Dir],
    _typeDirs :: Map ProjectType [Path Abs Dir],
    _projectTypes :: Map ProjectType [Path Abs Dir],
    _typeMap :: Map ProjectType [ProjectType],
    _typeMarkers :: Map ProjectType [Text],
    _langMap :: Map ProjectType ProjectLang,
    _langsMap :: Map ProjectLang [ProjectLang]
  }
  deriving (Generic, MsgpackDecode, MsgpackEncode)

makeClassy ''ProjectConfig
