module Proteome.Data.ResolveError where

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log.Logger (Priority(NOTICE))

newtype ResolveError =
  ParsePath Text
  deriving (Eq, Show)

deepPrisms ''ResolveError

instance ReportError ResolveError where
  errorReport (ParsePath path) =
    ErrorReport [text|invalid path: $path|] ["ResolveError.ParsePath:", path] NOTICE
