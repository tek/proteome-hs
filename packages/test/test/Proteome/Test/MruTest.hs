module Proteome.Test.MruTest where

import Hedgehog ((===))
import Ribosome.Api.Buffer (edit)
import Ribosome.Nvim.Api.IO (bufferGetName, vimCommand)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)

import Proteome.BufEnter (updateBuffers)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (buffers)
import Proteome.Test.Unit (ProteomeTest, testDef)

setupBuffers ::
  NvimE e m =>
  MonadIO m =>
  m (Text, Text, Text)
setupBuffers = do
  buf1 <- fixture "buffers/buf1"
  buf2 <- fixture "buffers/buf2"
  buf3 <- fixture "buffers/buf3"
  edit buf1
  edit buf2
  edit buf3
  return (toText buf1, toText buf2, toText buf3)

mruSpec :: ProteomeTest ()
mruSpec = do
  (buf1, buf2, buf3) <- setupBuffers
  edit (toString buf3)
  updateBuffers
  edit (toString buf2)
  updateBuffers
  edit (toString buf1)
  updateBuffers
  vimCommand "bdelete! 2"
  updateBuffers
  ([buf1, buf3] ===) =<< traverse bufferGetName =<< getL @Env Env.buffers

test_mru :: UnitTest
test_mru =
  testDef mruSpec
