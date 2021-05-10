module Proteome.Data.Replace where

import Prelude hiding (lines)
import Ribosome.Data.Scratch (Scratch)

import Proteome.Data.GrepOutputLine (GrepOutputLine)

data Replace =
  Replace {
    _scratch :: Scratch,
    _lines :: NonEmpty GrepOutputLine
  }
  deriving (Eq, Show)

makeClassy ''Replace
