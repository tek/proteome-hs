module Proteome.Data.ProjectName where

newtype ProjectName = ProjectName Text
  deriving (Eq, Show, Generic)
  deriving newtype (MsgpackEncode, MsgpackDecode, IsString)
