module Proteome.Plugin where

import Neovim (CommandOption (CmdBang), Neovim, NeovimPlugin, Plugin, wrapPlugin)
import Neovim.Plugin.Classes (CommandOption(CmdComplete))
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Plugin (RpcDef, autocmd, cmd, name, riboPlugin, rpcHandler, rpcHandlerDef, sync)

import Proteome.Add (proAdd, proAddCmd)
import Proteome.BufEnter (bufEnter)
import Proteome.Buffers (proBuffers)
import Proteome.Config (proReadConfig)
import Proteome.Data.Env (Env, Proteome)
import Proteome.Data.Error (Error)
import Proteome.Diag (proDiag)
import Proteome.Filename (proCopy, proMove, proRemove)
import Proteome.Files (proFiles)
import Proteome.Grep (proGrep, proGrepIn, proGrepList, proGrepOpt, proGrepOptIn)
import Proteome.Grep.Replace (proReplaceQuit, proReplaceSave)
import Proteome.Init (initialize, proteomeStage1, proteomeStage2, proteomeStage4)
import Proteome.Project.Activate (proNext, proPrev)
import Proteome.Quit (proQuit)
import Proteome.Save (proSave)
import Proteome.Tags (proTags)

handleError :: Error -> Proteome ()
handleError =
  reportError "proteome"

rpcHandlers :: [[RpcDef (Ribo Env Error)]]
rpcHandlers =
  [
    $(rpcHandler sync 'proteomeStage1),
    $(rpcHandler sync 'proteomeStage2),
    $(rpcHandler sync 'proteomeStage4),
    $(rpcHandler (name "ProAddProject") 'proAdd),
    $(rpcHandler (cmd [CmdBang] . name "ProAdd") 'proAddCmd),
    $(rpcHandlerDef 'proSave),
    $(rpcHandlerDef 'proTags),
    $(rpcHandler (cmd []) 'proGrep),
    $(rpcHandler (cmd []) 'proGrepIn),
    $(rpcHandler (cmd []) 'proGrepOpt),
    $(rpcHandler (cmd []) 'proGrepOptIn),
    $(rpcHandler sync 'proGrepList),
    $(rpcHandler (cmd []) 'proReplaceSave),
    $(rpcHandler (cmd []) 'proReplaceQuit),
    $(rpcHandler (cmd []) 'proBuffers),
    $(rpcHandler (cmd []) 'proFiles),
    $(rpcHandler (cmd []) 'proNext),
    $(rpcHandler (cmd []) 'proPrev),
    $(rpcHandler (cmd [CmdComplete "file"]) 'proMove),
    $(rpcHandler (cmd [CmdComplete "file"]) 'proCopy),
    $(rpcHandler (cmd []) 'proRemove),
    $(rpcHandlerDef 'proReadConfig),
    $(rpcHandler (cmd []) 'proDiag),
    $(rpcHandler (autocmd "BufEnter") 'bufEnter),
    $(rpcHandler (autocmd "BufWritePost") 'proSave),
    $(rpcHandler (autocmd "VimLeave" . sync) 'proQuit)
  ]


plugin' :: Ribosome Env -> Plugin (Ribosome Env)
plugin' env =
  riboPlugin "proteome" env rpcHandlers def handleError def

plugin :: Neovim e NeovimPlugin
plugin =
  wrapPlugin . plugin' =<< initialize
