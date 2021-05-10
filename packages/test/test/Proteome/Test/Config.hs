module Proteome.Test.Config where

import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)
import Ribosome.Test.Embed (TestConfig(tcVariables), Vars, varsFromList, varsUnion)
import System.Directory (getCurrentDirectory)

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = E.defaultTestConfigWith "proteome"

defaultTestConfig :: TestConfig
defaultTestConfig = E.defaultTestConfig "proteome"

vars ::
  MonadIO m =>
  m Vars
vars = do
  base <- liftIO getCurrentDirectory
  pure $ varsFromList [
    ("proteome_project_base_dirs", toMsgpack @[String] [base <> "/test/fixtures/projects"]),
    ("proteome_main_project_dir", toMsgpack $ base <> "/test/fixtures/projects/haskell/flagellum")
    ]

withVars :: TestConfig -> IO TestConfig
withVars conf = do
  v <- vars
  return conf { tcVariables = varsUnion v (tcVariables conf) }
