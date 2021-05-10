module Proteome.Data.ProjectType where

newtype ProjectType =
  ProjectType { _tpe :: Text }
  deriving (Ord, Eq, Show, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)

makeClassy ''ProjectType
