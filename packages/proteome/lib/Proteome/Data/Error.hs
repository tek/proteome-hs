module Proteome.Data.Error where

import Ribosome.Data.RiboError (RiboError)

import Proteome.Data.AddError (AddError)
import Proteome.Data.FilenameError (FilenameError)
import Proteome.Data.FilesError (FilesError)
import Proteome.Data.GrepError (GrepError)
import Proteome.Data.ReplaceError (ReplaceError)
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Data.TagsError (TagsError)

data Error =
  Rib RiboError
  |
  Add AddError
  |
  Tags TagsError
  |
  Grep GrepError
  |
  Replace ReplaceError
  |
  Resolve ResolveError
  |
  Files FilesError
  |
  Filename FilenameError
  deriving (Show, Generic, ReportError)

deepPrisms ''Error
