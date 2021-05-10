module Proteome.Data.ProjectLang where

newtype ProjectLang =
  ProjectLang { _lang :: Text }
  deriving (Ord, Eq, Show, Generic)
  deriving newtype (MsgpackEncode, MsgpackDecode, IsString)

makeClassy ''ProjectLang
