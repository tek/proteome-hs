module Proteome.Buffers where

import Control.Lens (each, elemOf, view)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Foldable (maximum)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (length, replicate, stripPrefix)
import Ribosome.Api.Buffer (bufferIsFile, buflisted, setCurrentBuffer)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Api.Window (ensureMainWindow)
import Ribosome.Config.Setting (settingOr)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Menu.Action (menuContinue, menuFilter, menuQuitWith)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Run (strictNvimMenu)
import Ribosome.Menu.Simple (
  defaultMenu,
  deleteMarked,
  markedMenuItems,
  unmarkedMenuItems,
  withSelectedMenuItem,
  )
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (
  bufferGetName,
  bufferGetNumber,
  nvimBufIsLoaded,
  vimCommand,
  vimGetCurrentBuffer,
  vimGetCurrentWindow,
  vimSetCurrentWindow,
  )

import Proteome.Buffers.Syntax (buffersSyntax)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (buffers)
import Proteome.Data.ListedBuffer (ListedBuffer(ListedBuffer))
import qualified Proteome.Data.ListedBuffer as ListedBuffer (buffer, number)
import qualified Proteome.Settings as Settings (buffersCurrentLast)

action ::
  NvimE e m =>
  (MenuItem ListedBuffer -> m ()) ->
  Menu ListedBuffer ->
  m (MenuConsumerAction m (), Menu ListedBuffer)
action act menu =
  withSelectedMenuItem quit menu
  where
    quit item =
      menuQuitWith (act item) menu

loadListedBuffer ::
  NvimE e m =>
  ListedBuffer ->
  m ()
loadListedBuffer (ListedBuffer buffer number _) =
  ifM (nvimBufIsLoaded buffer) (setCurrentBuffer buffer) (vimCommand $ "buffer " <> show number)

load ::
  NvimE e m =>
  Menu ListedBuffer ->
  Prompt ->
  m (MenuConsumerAction m (), Menu ListedBuffer)
load menu _ =
  action quit menu
  where
    quit (MenuItem buf _ _) =
      loadListedBuffer buf

compensateForMissingActiveBuffer ::
  NvimE e m =>
  NonEmpty ListedBuffer ->
  [ListedBuffer] ->
  m ()
compensateForMissingActiveBuffer _ [] =
  vimCommand "enew"
compensateForMissingActiveBuffer marked (next : _) = do
  prev <- vimGetCurrentWindow
  void ensureMainWindow
  current <- vimGetCurrentBuffer
  when (elemOf (each . ListedBuffer.buffer) current marked) (loadListedBuffer next)
  vimSetCurrentWindow prev

deleteListedBuffersWith ::
  NvimE e m =>
  Text ->
  NonEmpty ListedBuffer ->
  m ()
deleteListedBuffersWith deleter bufs =
  vimCommand $ deleter <> " " <> numbers
  where
    numbers =
      unwords (show . view ListedBuffer.number <$> NonEmpty.toList bufs)

deleteWith ::
  NvimE e m =>
  Text ->
  Menu ListedBuffer ->
  Prompt ->
  m (MenuConsumerAction m (), Menu ListedBuffer)
deleteWith deleter menu _ =
  maybe (menuContinue menu) delete marked
  where
    marked =
      fmap (view MenuItem.meta) <$> markedMenuItems menu
    remaining =
      view MenuItem.meta <$> unmarkedMenuItems menu
    delete bufs = do
      compensateForMissingActiveBuffer bufs remaining
      deleteListedBuffersWith deleter bufs
      menuFilter (deleteMarked menu)

moveCurrentLast ::
  NvimE e m =>
  [MenuItem ListedBuffer] ->
  m [MenuItem ListedBuffer]
moveCurrentLast items = do
  current <- vimGetCurrentBuffer
  return $ spin current items []
  where
    spin current (item : rest) result | view lens item == current =
      result ++ rest ++ [item]
    spin current (item : rest) result =
      spin current rest (item : result)
    spin _ [] result =
      result
    lens =
      MenuItem.meta . ListedBuffer.buffer

buffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  m [MenuItem ListedBuffer]
buffers = do
  cwd <- (<> "/") <$> nvimCwd
  bufs <- filterM bufferIsFile =<< filterM buflisted =<< getL @Env Env.buffers
  numbers <- traverse bufferGetNumber bufs
  names <- traverse bufferGetName bufs
  let items = item (toText cwd) (padding numbers) <$> zip3 bufs numbers names
  ifM (settingOr False Settings.buffersCurrentLast) (moveCurrentLast items) (return items)
  where
    padding =
      Text.length . show . maximum
    item cwd pad (buf, num, name) =
      MenuItem (ListedBuffer buf num name) text' text'
      where
        text' =
          " * " <> padded pad (show num) <> "  " <> strip cwd name
    padded pad num =
      Text.replicate (pad - Text.length num) " " <> num
    strip cwd name =
      fromMaybe name $ Text.stripPrefix cwd name

actions ::
  NvimE e m =>
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
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  PromptConfig m ->
  m ()
buffersWith promptConfig = do
  bufs <- buffers
  void $ strictNvimMenu scratchOptions bufs handler promptConfig Nothing
  where
    scratchOptions =
      scratchSyntax [buffersSyntax] . defaultScratchOptions $ "proteome-buffers"
    handler =
      defaultMenu (Map.fromList actions)

proBuffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  m ()
proBuffers =
  buffersWith (defaultPrompt [])
