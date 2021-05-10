module Proteome.Grep.Replace where

import Control.Lens (view)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Text as Text
import Ribosome.Api.Autocmd (bufferAutocmd)
import Ribosome.Api.Buffer (addBuffer, bufferContent, bufferForFile, wipeBuffer)
import Ribosome.Api.Option (withOption)
import Ribosome.Api.Window (closeWindow)
import Ribosome.Data.FloatOptions (FloatOptions(FloatOptions))
import Ribosome.Data.Scratch (Scratch(Scratch))
import qualified Ribosome.Data.Scratch as Scratch (Scratch(scratchBuffer))
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchFocus, scratchModify)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer, Window)
import Ribosome.Nvim.Api.IO (bufferGetLines, bufferSetLines, bufferSetOption, nvimWinSetBuf, vimCommand)
import Ribosome.Scratch (createFloat, showInScratch)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (replace)
import Proteome.Data.GrepOutputLine (GrepOutputLine(GrepOutputLine))
import qualified Proteome.Data.GrepOutputLine as GrepOutputLine (content)
import Proteome.Data.Replace (Replace(Replace))
import Proteome.Data.ReplaceError (ReplaceError)
import qualified Proteome.Data.ReplaceError as ReplaceError (ReplaceError(BadReplacement, CouldntLoadBuffer))

scratchName :: Text
scratchName =
  "proteome-replace"

replaceBuffer ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  NonEmpty (MenuItem GrepOutputLine) ->
  m ()
replaceBuffer items = do
  scratch <- showInScratch content options
  let buffer = Scratch.scratchBuffer scratch
  bufferSetOption buffer "buftype" (toMsgpack ("acwrite" :: Text))
  bufferAutocmd buffer "ProteomeReplaceSave" "BufWriteCmd" "silent! ProReplaceSave"
  bufferAutocmd buffer "ProteomeReplaceQuit" "BufUnload" "silent! ProReplaceQuit"
  setL @Env Env.replace (Just (Replace scratch lines'))
  where
    content =
      view GrepOutputLine.content <$> lines'
    lines' =
      view MenuItem.meta <$> items
    options =
      scratchModify . scratchFocus . defaultScratchOptions $ scratchName

-- If the deleted line was surrounded by blank lines or buffer edges, there will be extraneous whitespace.
-- First check whether the line number of the deleted line was line 0 and its content is now empty.
-- Then do the same for the last line.
-- Finally, check if both the preceding and current line are empty.
deleteExtraBlankLine ::
  MonadIO m =>
  NvimE e m =>
  Buffer ->
  Int ->
  m ()
deleteExtraBlankLine buffer line = do
  check (line - 2) line [""]
  check line (line + 1) [""]
  check (line - 1) line ["", ""]
  where
    check l r target = do
      content <- readLines l (r + 1)
      when (content == target) delete
    readLines l r =
      bufferGetLines buffer (clamp0 l) r False
    delete =
      bufferSetLines buffer line (line + 1) False []
    clamp0 a | a < 0 = 0
    clamp0 a = a

replaceLine ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  Window ->
  Text ->
  GrepOutputLine ->
  m (Maybe Buffer)
replaceLine window updatedLine (GrepOutputLine path line _ _) = do
  exists <- isJust <$> bufferForFile path
  unless exists $ addBuffer path
  buffer <- hoistMaybe (ReplaceError.CouldntLoadBuffer path) =<< bufferForFile path
  nvimWinSetBuf window buffer
  bufferSetLines buffer line (line + 1) False replacement
  deleteExtraBlankLine buffer line
  return $ if exists then Nothing else Just buffer
  where
    replacement =
      if Text.null updatedLine
      then []
      else [updatedLine]

lineNumberDesc :: (Text, GrepOutputLine) -> Int
lineNumberDesc (_, GrepOutputLine _ number _ _) =
  -number

replaceFloatOptions :: FloatOptions
replaceFloatOptions =
  FloatOptions def 1 1 0 0 True def Nothing

withReplaceFloat ::
  NvimE e m =>
  MonadBaseControl IO m =>
  (Window -> m [Maybe Buffer]) ->
  m ()
withReplaceFloat consumer = do
  (buffer, window) <- createFloat replaceFloatOptions
  transient <- withOption "hidden" True (consumer window)
  ignoreError @RpcError $ vimCommand "noautocmd wall"
  traverse_ wipeBuffer (catMaybes transient)
  closeWindow window
  wipeBuffer buffer

replaceLines ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  Buffer ->
  [(Text, GrepOutputLine)] ->
  m ()
replaceLines scratchBuffer lines' = do
  withReplaceFloat \ window -> do
    transient <- traverse (uncurry (replaceLine window)) (sortOn lineNumberDesc lines')
    bufferSetOption scratchBuffer "buftype" (toMsgpack ("nofile" :: Text))
    pure transient
  bufferSetOption scratchBuffer "buftype" (toMsgpack ("acwrite" :: Text))
  bufferSetOption scratchBuffer "modified" (toMsgpack False)

deleteLines ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  [GrepOutputLine] ->
  m ()
deleteLines lines' =
  withReplaceFloat \ window ->
    traverse (uncurry (replaceLine window)) withReplacement
  where
    withReplacement =
      sortOn lineNumberDesc (zip (repeat "") lines')

replaceSave ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  Replace ->
  m ()
replaceSave (Replace (Scratch _ buffer _ _ _) lines') = do
  updatedLines <- bufferContent buffer
  if length updatedLines /= length lines'
  then badReplacement
  else replaceLines buffer (zip updatedLines (NonEmpty.toList lines'))
  where
    badReplacement =
      throwHoist ReplaceError.BadReplacement

proReplaceSave ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  m ()
proReplaceSave =
  traverse_ replaceSave =<< getL @Env Env.replace

proReplaceQuit ::
  MonadDeepState s Env m =>
  m ()
proReplaceQuit =
  setL @Env Env.replace Nothing
