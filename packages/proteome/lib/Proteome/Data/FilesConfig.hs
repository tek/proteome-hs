module Proteome.Data.FilesConfig where

import Text.RE.PCRE.Text (RE)

data FilesConfig =
  FilesConfig {
    _useRg :: Bool,
    _ignoreHidden :: Bool,
    _ignoreFiles :: [RE],
    _ignoreDirs :: [RE],
    _wildignore :: [Text]
  }

makeClassy ''FilesConfig
