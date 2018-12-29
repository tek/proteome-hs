{-# OPTIONS_GHC -F -pgmF htfpp #-}

module InitSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Test.Framework
import Ribosome.Control.Ribo (Ribo)
import Ribosome.Config.Setting (setting)
import qualified Proteome.Settings as S
import Proteome.Data.Project (ProjectName(ProjectName), ProjectType(ProjectType))
import Proteome.Test.Functional (specWith)
import Config (vars)

initSpec :: Ribo env ()
initSpec = do
  tpe <- setting S.mainType
  name <- setting S.mainName
  liftIO $ assertEqual name (ProjectName "flagellum")
  liftIO $ assertEqual tpe (ProjectType "haskell")

test_init :: IO ()
test_init =
  vars >>= specWith initSpec
