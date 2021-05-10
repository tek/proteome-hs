module Proteome.Add where

import Conduit (ConduitT, yield)
import Control.Lens (view)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text
import Neovim (CommandArguments(CommandArguments))
import Path (Abs, Dir, Path, dirname, parent, stripProperPrefix)
import Path.IO (listDir)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu.Action (menuContinue, menuQuitWith)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem, MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, PromptFlag(StartInsert))
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, markedMenuItems)
import Ribosome.Msgpack.Error (DecodeError)

import Proteome.Add.Syntax (addSyntax)
import Proteome.Data.AddError (AddError)
import qualified Proteome.Data.AddError as AddError (AddError(InvalidProjectSpec))
import Proteome.Data.AddItem (AddItem(AddItem))
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (projects)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectConfig (ProjectConfig)
import qualified Proteome.Data.ProjectConfig as ProjectConfig
import Proteome.Data.ProjectMetadata (ProjectMetadata(VirtualProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Path (dropSlash, pathText)
import Proteome.Project.Activate (selectProject)
import Proteome.Project.Resolve (resolveProjectFromConfig)
import qualified Proteome.Settings as Settings

add ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  ProjectName ->
  Maybe ProjectType ->
  Bool ->
  m ()
add name tpe activate = do
  addDirProject =<< resolveProjectFromConfig Nothing name tpe
  when activate $ selectProject (-1)
  where
    addDirProject (Project (VirtualProject _) _ _ _) =
      return ()
    addDirProject project =
      modifyL @Env Env.projects (\p -> p ++ [project])

proAdd ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  AddOptions ->
  m ()
proAdd (AddOptions name tpe activate) =
  add name (Just tpe) (fromMaybe False activate)

addFromName ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  ProjectName ->
  Bool ->
  m ()
addFromName name =
  add name Nothing

proAddCmd ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e AddError m =>
  MonadDeepError e ResolveError m =>
  CommandArguments ->
  Text ->
  m ()
proAddCmd (CommandArguments bang _ _ _) spec =
  process (Text.splitOn "/" spec)
  where
    process [tpe, name] =
      add (ProjectName name) (Just (ProjectType tpe)) activate
    process [name] =
      addFromName (ProjectName name) activate
    process _ =
      throwHoist (AddError.InvalidProjectSpec spec)
    activate =
      fromMaybe False bang

availableProjectsInBase ::
  MonadIO m =>
  Path Abs Dir ->
  m [MenuItem AddItem]
availableProjectsInBase base =
  fmap (fmap cons . join) . traverse list =<< list base
  where
    list =
      fmap fst . listDir
    cons proj =
      MenuItem (AddItem tpe name) pt (fromMaybe pt (pathText <$> stripProperPrefix proj base))
      where
        tpe =
          dropSlash (dirname (parent proj))
        name =
          dropSlash (dirname proj)
        pt =
          pathText proj

availableProjects ::
  MonadIO m =>
  ProjectConfig ->
  m [MenuItem AddItem]
availableProjects (view ProjectConfig.baseDirs -> dirs) =
  join <$> traverse availableProjectsInBase dirs

availableProjectsC ::
  MonadIO m =>
  ProjectConfig ->
  ConduitT () [MenuItem AddItem] m ()
availableProjectsC =
  yield <=< lift . availableProjects

menuAdd ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  Menu AddItem ->
  Prompt ->
  m (MenuConsumerAction m (), Menu AddItem)
menuAdd menu _ =
  action menu
  where
    action =
      maybe menuContinue quit marked
    quit =
      menuQuitWith . traverse_ run
    run (AddItem tpe name) =
      add (ProjectName name) (Just (ProjectType tpe)) True
    marked =
      fmap (view MenuItem.meta) <$> markedMenuItems menu

actions ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  MonadBaseControl IO m =>
  [(Text, Menu AddItem -> Prompt -> m (MenuConsumerAction m (), Menu AddItem))]
actions =
  [
    ("cr", menuAdd)
    ]

addMenuWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  MonadDeepError e DecodeError m =>
  PromptConfig m ->
  m ()
addMenuWith promptConfig = do
  projectConfig <- setting Settings.projectConfig
  void $ nvimMenu scratchOptions (availableProjectsC projectConfig) handler promptConfig Nothing
  where
    scratchOptions =
      scratchSyntax [addSyntax] . defaultScratchOptions $ "proteome-add"
    handler =
      defaultMenu (Map.fromList actions)

proAddMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  MonadDeepError e DecodeError m =>
  m ()
proAddMenu =
  addMenuWith (defaultPrompt [StartInsert])
