module Proteome.Data.FilesError where

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log.Logger (Priority(ERROR, NOTICE))

data FilesError =
  BadCwd
  |
  NoSuchPath Text
  |
  BadRE Text Text
  deriving (Eq, Show)

deepPrisms ''FilesError

instance ReportError FilesError where
  errorReport BadCwd =
    ErrorReport "internal error" ["FilesError.BadCwd"] ERROR
  errorReport (NoSuchPath path) =
    ErrorReport ("path doesn't exist: " <> path) ["FilesError.NoSuchPath:", path] NOTICE
  errorReport (BadRE var re) =
    ErrorReport ("bad regex in `g:proteome_" <> var <> "`: " <> re) ["FilesError.BadRE:", var, re] NOTICE
