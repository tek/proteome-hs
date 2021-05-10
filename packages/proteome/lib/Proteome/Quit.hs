module Proteome.Quit where

import Control.Monad.Catch (MonadThrow)
import Ribosome.Data.SettingError (SettingError)

import Proteome.Data.Env (Env)
import Proteome.PersistBuffers (storeBuffers)

proQuit ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  m ()
proQuit =
  storeBuffers
