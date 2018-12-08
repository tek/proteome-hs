module Ribosome.Data.Ribo(
  Ribo,
  state,
  inspect,
  modify,
) where

import Control.Concurrent.STM.TVar (modifyTVar)
import UnliftIO.STM (TVar, atomically, readTVarIO)
import Neovim (Neovim, ask)
import Ribosome.Data.Ribosome

type Ribo e = Neovim (Ribosome e)

state :: Ribo (TVar e) e
state = do
  Ribosome _ t <- ask
  readTVarIO t

inspect :: (e -> a) -> Ribo (TVar e) a
inspect f = fmap f state

modify :: (e -> e) -> Ribo (TVar e) ()
modify f = do
  Ribosome _ t <- ask
  atomically $ modifyTVar t f
