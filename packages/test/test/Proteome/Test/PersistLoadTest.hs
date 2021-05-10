module Proteome.Test.PersistLoadTest where

import qualified Data.Text.IO as Text (writeFile)
import Hedgehog ((===))
import Path (absdir)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (persistenceDir)
import Ribosome.Nvim.Api.IO (bufferGetName, vimGetBuffers, vimGetCurrentBuffer, vimSetOption)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture, tempDir)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (buffers, mainProject)
import qualified Proteome.Data.Project as Project (meta)
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.PersistBuffers (loadBuffers)
import Proteome.Test.Config (vars)
import Proteome.Test.Unit (ProteomeTest, testWithDef)

main :: ProjectMetadata
main = DirProject (ProjectName "flagellum") (ProjectRoot [absdir|/|]) (Just (ProjectType "haskell"))

buffersJson :: Text -> Text
buffersJson base =
  [text|{"current":"#{base}/file2","buffers":["#{base}/file1","#{base}/file2","#{base}/file3"]}|]

loadBuffersTest :: ProteomeTest ()
loadBuffersTest = do
  vimSetOption "swapfile" (toMsgpack False)
  setL @Env (Env.mainProject . Project.meta) main
  persistBase <- tempDir "persist/load"
  let persistDir = persistBase </> "proteome/haskell/flagellum"
  liftIO $ createDirectoryIfMissing True persistDir
  fixDir <- fixture "persist/store"
  liftIO $ Text.writeFile (persistDir </> "buffers.json") (buffersJson (toText fixDir))
  updateSetting persistenceDir persistBase
  loadBuffers
  buffers <- vimGetBuffers
  3 === length buffers
  active <- bufferGetName =<< vimGetCurrentBuffer
  toString active === fixDir </> "file2"
  (target fixDir ===) =<< traverse bufferGetName =<< getL @Env Env.buffers
  where
    target fixDir =
      toText . (fixDir </>) <$> ["file1", "file2", "file3"]

test_loadPersistedBuffers :: UnitTest
test_loadPersistedBuffers =
  vars >>= testWithDef loadBuffersTest
