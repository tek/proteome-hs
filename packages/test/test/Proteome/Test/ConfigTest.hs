module Proteome.Test.ConfigTest where

import Data.MessagePack (Object(ObjectInt))
import Ribosome.Api.Option (rtpCat)
import Ribosome.Control.Ribosome (newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimGetVar)
import Ribosome.Test.Await (awaitEqual_)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)
import System.Directory (getCurrentDirectory)

import Proteome.Init (resolveAndInitMain)
import Proteome.Plugin (plugin')
import Proteome.Test.Unit (ProteomeTest, integrationTestDef)

configTest :: ProteomeTest ()
configTest = do
  base <- liftIO getCurrentDirectory
  rtpCat [text|#{base}/test/fixtures/rtp|]
  dir <- fixture "projects/haskell/flagellum"
  vimCommand $ "cd " <> toText dir
  resolveAndInitMain
  () <- vimCallFunction "ProReadConfig" []
  awaitEqual_ (ObjectInt 13) (vimGetVar "flag")

test_config :: UnitTest
test_config = do
  ribo <- newRibosome "proteome" def
  integrationTestDef (plugin' ribo) configTest
