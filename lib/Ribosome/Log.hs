module Ribosome.Log(
  debug,
  info,
) where

import Control.Monad.IO.Class (liftIO)
import Neovim.Log (debugM, infoM)
import Ribosome.Data.Ribo (Ribo)

debug :: Show a => String -> a -> Ribo e ()
debug name message = liftIO $ debugM name $ show message

info :: Show a => String -> a -> Ribo e ()
info name message = liftIO $ infoM name $ show message
