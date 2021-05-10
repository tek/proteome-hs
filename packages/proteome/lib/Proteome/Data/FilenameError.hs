module Proteome.Data.FilenameError where

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log.Logger (Priority(ERROR, NOTICE))

data FilenameError =
  BadCwd
  |
  InvalidPathSpec Text
  |
  BufferPathInvalid
  |
  CreateDir Text
  |
  Exists Text
  |
  ActionFailed Text Text
  deriving (Eq, Show, Generic)

deepPrisms ''FilenameError

instance ReportError FilenameError where
  errorReport = \case
    BadCwd ->
      ErrorReport "could not determine current directory" ["FilenameError.BadCwd"] ERROR
    InvalidPathSpec spec ->
      ErrorReport [text|invalid path: #{spec}|] ["FilenameError.InvalidPathSpec:", spec] NOTICE
    BufferPathInvalid ->
      ErrorReport "current buffer is not an existing file" ["FilenameError.BufferPathInvalid"] NOTICE
    CreateDir dir ->
      ErrorReport [text|could not create directory #{dir}|] ["FilenameError.CreateDir:", dir] NOTICE
    Exists file ->
      ErrorReport [text|file already exists: #{file}|] ["FilenameError.Exists:", file] NOTICE
    ActionFailed action err ->
      ErrorReport "filesystem error" ["FilenameError.ActionFailed:", action, err] NOTICE
