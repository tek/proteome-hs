module Proteome.Test.PersistStoreTest where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B (readFile)
import Hedgehog ((===))
import Path (Abs, File, Path, absdir, parseAbsDir, relfile, toFilePath, (</>))
import Ribosome.Api.Buffer (edit)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (persistenceDir)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture, tempDir)

import Proteome.BufEnter (updateBuffers)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (mainProject)
import Proteome.Data.PersistBuffers (PersistBuffers(PersistBuffers))
import qualified Proteome.Data.Project as Project (meta)
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.PersistBuffers (storeBuffers)
import Proteome.Test.Config (vars)
import Proteome.Test.Unit (ProteomeTest, testWithDef)

main :: ProjectMetadata
main = DirProject (ProjectName "flagellum") (ProjectRoot [absdir|/|]) (Just (ProjectType "haskell"))

storeTarget :: Path Abs File -> Path Abs File -> Path Abs File -> PersistBuffers
storeTarget f1 f2 f3 =
  PersistBuffers (Just f2) [f2, f3, f1]

storeBuffersSpec :: ProteomeTest ()
storeBuffersSpec = do
  let nonexistentFile = [relfile|nonexistent|]
  base <- parseAbsDir =<< fixture "persist/store"
  let file1 = base </> [relfile|file1|]
  let file2 = base </> [relfile|file2|]
  let file3 = base </> [relfile|file3|]
  e file1
  e file2
  e file3
  e $ base </> nonexistentFile
  e file2
  persistDir <- parseAbsDir =<< tempDir "persist/store"
  setL @Env (Env.mainProject . Project.meta) main
  updateSetting persistenceDir (toFilePath persistDir)
  storeBuffers
  let bufferFile = persistDir </> [relfile|proteome/haskell/flagellum/buffers.json|]
  json <- liftIO $ B.readFile (toFilePath bufferFile)
  pb <- case decode json of
    Just a -> return a
    _ -> fail "invalid json"
  storeTarget file1 file2 file3 === pb
  where
    e a =
      edit (toFilePath a) *> updateBuffers

test_storeBuffers :: UnitTest
test_storeBuffers =
  vars >>= testWithDef storeBuffersSpec
