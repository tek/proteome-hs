module Proteome.Data.ReplaceError where

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log (Priority(NOTICE, ERROR))

data ReplaceError =
  BadReplacement
  |
  CouldntLoadBuffer Text
  deriving (Eq, Show)

deepPrisms ''ReplaceError

instance ReportError ReplaceError where
  errorReport BadReplacement =
    ErrorReport "replacment line count does not match original" ["ReplaceError.BadReplacement"] NOTICE
  errorReport (CouldntLoadBuffer path) =
    ErrorReport ("could not load file " <> path) ["ReplaceError.CouldntLoadBuffer", path] ERROR
