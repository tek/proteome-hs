module Proteome.Data.GrepOutputLine where

data GrepOutputLine =
  GrepOutputLine {
    _path :: Text,
    _line :: Int,
    _col :: Maybe Int,
    _content :: Text
  }
  deriving (Eq, Show, Generic, MsgpackEncode)

makeClassy ''GrepOutputLine
