module Proteome.Data.AddError where

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log.Logger (Priority(NOTICE))

newtype AddError =
  InvalidProjectSpec Text
  deriving (Eq, Show)

deepPrisms ''AddError

instance ReportError AddError where
  errorReport (InvalidProjectSpec spec) =
    ErrorReport ("no such project: " <> spec) ["AddError.InvalidProjectSpec:", spec] NOTICE
