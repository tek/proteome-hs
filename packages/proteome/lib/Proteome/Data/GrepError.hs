module Proteome.Data.GrepError where

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log (Priority(NOTICE))

data GrepError =
  Empty
  |
  NotInPath Text
  |
  NoSuchExecutable Text
  |
  NoSuchDestination Text
  |
  EmptyPattern
  deriving (Eq, Show)

deepPrisms ''GrepError

instance ReportError GrepError where
  errorReport Empty =
    ErrorReport "grep cmdline is empty" ["GrepError.Empty"] NOTICE
  errorReport (NotInPath exe) =
    ErrorReport ("grep executable `" <> exe <> "` not found in $PATH") ["GrepError.NotInPath:", exe] NOTICE
  errorReport (NoSuchExecutable exe) =
    ErrorReport ("grep executable `" <> exe <> "` does not exist") ["GrepError.NoSuchExecutable:", exe] NOTICE
  errorReport (NoSuchDestination dir) =
    ErrorReport ("grep destination `" <> dir <> "` does not exist") ["GrepError.NoSuchDestination:", dir] NOTICE
  errorReport EmptyPattern =
    ErrorReport "no pattern given" ["GrepError.EmptyPattern"] NOTICE
