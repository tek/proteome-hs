module Proteome.Data.Env where

import Ribosome.Data.Errors (Errors)
import Ribosome.Nvim.Api.Data (Buffer)

import Proteome.Data.Error (Error)
import Proteome.Data.Project(Project)
import Proteome.Data.Replace (Replace)

data Env =
  Env {
    _mainProject :: Project,
    _projects :: [Project],
    _errors :: Errors,
    _currentProjectIndex :: Int,
    _configLog :: [Text],
    _buffers :: [Buffer],
    _replace :: Maybe Replace
  }
  deriving (Show, Generic, Default)

deepLenses ''Env

type Proteome a = Ribo Env Error a
