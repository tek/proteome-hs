module Proteome.Buffers where

import Conduit (ConduitT, yield)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (length, replicate)
import Ribosome.Api.Buffer (bufferIsFile, buflisted, setCurrentBuffer)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, menuQuitWith, withSelectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (bufferGetName, bufferGetNumber, vimCommand)

import Proteome.Buffers.Syntax (buffersSyntax)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (buffers)
import Proteome.Data.ListedBuffer (ListedBuffer(ListedBuffer))

action ::
  NvimE e m =>
  MonadRibo m =>
  (MenuItem ListedBuffer -> m ()) ->
  Menu ListedBuffer ->
  m (MenuConsumerAction m (), Menu ListedBuffer)
action act menu =
  withSelectedMenuItem quit menu
  where
    quit item =
      menuQuitWith (act item) menu

load ::
  NvimE e m =>
  MonadRibo m =>
  Menu ListedBuffer ->
  Prompt ->
  m (MenuConsumerAction m (), Menu ListedBuffer)
load menu _ =
  action quit menu
  where
    quit (MenuItem (ListedBuffer buffer _ _) _) =
      setCurrentBuffer buffer

deleteWith ::
  NvimE e m =>
  MonadRibo m =>
  Text ->
  Menu ListedBuffer ->
  Prompt ->
  m (MenuConsumerAction m (), Menu ListedBuffer)
deleteWith deleter menu _ =
  action handle menu
  where
    handle (MenuItem (ListedBuffer _ number _) _) =
      vimCommand $ deleter <> " " <> show number

buffers ::
  NvimE e m =>
  MonadDeepState s Env m =>
  m [MenuItem ListedBuffer]
buffers = do
  bufs <- filterM bufferIsFile =<< filterM buflisted =<< getL @Env Env.buffers
  traverse (cons (Text.length . show . length $ bufs)) bufs
  where
    cons pad buf =
      item pad buf <$> bufferGetNumber buf <*> bufferGetName buf
    item pad buf num name =
      MenuItem (ListedBuffer buf num name) (" * " <> padded pad (show num) <> "  " <> name)
    padded pad num =
      Text.replicate (pad - Text.length num) " " <> num

bufferSource ::
  NvimE e m =>
  MonadDeepState s Env m =>
  ConduitT () [MenuItem ListedBuffer] m ()
bufferSource =
  yield =<< lift buffers

actions ::
  NvimE e m =>
  MonadRibo m =>
  [(Text, Menu ListedBuffer -> Prompt -> m (MenuConsumerAction m (), Menu ListedBuffer))]
actions =
  [
    ("cr", load),
    ("d", deleteWith "bdelete"),
    ("D", deleteWith "bdelete!"),
    ("w", deleteWith "bwipeout"),
    ("W", deleteWith "bwipeout!")
    ]

buffersWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  PromptConfig m ->
  m ()
buffersWith promptConfig =
  void $ nvimMenu scratchOptions bufferSource handler promptConfig Nothing
  where
    scratchOptions =
      scratchSyntax [buffersSyntax] . defaultScratchOptions $ "proteome-buffers"
    handler =
      defaultMenu (Map.fromList actions)

proBuffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  m ()
proBuffers =
  buffersWith (defaultPrompt False)
