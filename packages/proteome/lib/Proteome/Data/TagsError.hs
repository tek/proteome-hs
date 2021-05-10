module Proteome.Data.TagsError where

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log (Priority(NOTICE))

newtype TagsError =
  Path Text
  deriving (Eq, Show)

deepPrisms ''TagsError

instance ReportError TagsError where
  errorReport (Path msg) =
    ErrorReport msg ["TagsError.Path", msg] NOTICE
