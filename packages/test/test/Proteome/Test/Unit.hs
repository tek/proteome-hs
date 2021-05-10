module Proteome.Test.Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Neovim.Plugin (Plugin)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import qualified Ribosome.Test.Embed as Ribosome (integrationTest)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiTest, tmuxTest)

import Proteome.Data.Env (Env, Proteome)
import Proteome.Test.Config (defaultTestConfig, defaultTestConfigWith, withVars)
import Hedgehog (TestT)
import Proteome.Data.Error (Error)
import Ribosome.Test.Tmux (RiboTesting)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Test.Embed (TestConfig, Vars)
import Ribosome.Test.Unit (unitTest)

type ProteomeTest a =
  TestT (Ribo Env Error) a
type ProteomeTestingEnv env m n =
  RiboTesting Error env m n
type ProteomeTesting m n =
  ProteomeTestingEnv (Ribosome Env) m n

test ::
  ProteomeTesting m n =>
  Env ->
  TestT n a ->
  TestT m a
test =
  unitTest defaultTestConfig

testWith ::
  ProteomeTesting m n =>
  Env ->
  TestT n a ->
  Vars ->
  TestT m a
testWith env thunk vars' =
  unitTest (defaultTestConfigWith vars') env thunk

testWithDef ::
  ProteomeTesting m n =>
  TestT n a ->
  Vars ->
  TestT m a
testWithDef =
  testWith def

testDef ::
  ProteomeTesting m n =>
  TestT n a ->
  TestT m a
testDef thunk =
  testWithDef thunk def

withTmux ::
  Proteome () ->
  TmuxNative ->
  Proteome ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- updateSetting tmuxSocket socket
  thunk
withTmux _ _ = fail "no socket in test tmux"

tmuxTest ::
  ProteomeTesting m n =>
  TestT n a ->
  TestT m a
tmuxTest =
  Ribosome.tmuxTest defaultTestConfig def

tmuxGuiTest ::
  ProteomeTesting m n =>
  TestT n a ->
  TestT m a
tmuxGuiTest =
  Ribosome.tmuxGuiTest defaultTestConfig def

integrationTest ::
  ProteomeTestingEnv env m n =>
  (TestConfig -> IO TestConfig) ->
  Plugin env ->
  TestT n a ->
  TestT m a
integrationTest reconf plug thunk = do
  conf <- liftIO (reconf defaultTestConfig)
  Ribosome.integrationTest conf plug thunk

integrationTestDef ::
  ProteomeTestingEnv env m n =>
  Plugin env ->
  TestT n a ->
  TestT m a
integrationTestDef =
  integrationTest withVars
