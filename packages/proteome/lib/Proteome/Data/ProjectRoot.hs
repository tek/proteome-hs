module Proteome.Data.ProjectRoot where

import Path (Abs, Dir, Path)

newtype ProjectRoot = ProjectRoot (Path Abs Dir)
  deriving (Eq, Show, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode)
