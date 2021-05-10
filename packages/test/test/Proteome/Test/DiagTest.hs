module Proteome.Test.DiagTest where

import Hedgehog ((===))
import Path (Abs, Dir, Path, absdir, toFilePath)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Test.Run (UnitTest)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (configLog, mainProject)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Diag (proDiag)
import Proteome.Test.Config (vars)
import Proteome.Test.Project (ag, flag, fn, hask, idr, l, la, li, ti, tp)
import Proteome.Test.Unit (ProteomeTest, testWithDef)

root :: Path Abs Dir
root = [absdir|/projects/flagellum|]

main :: Project
main = Project (DirProject fn (ProjectRoot root) (Just tp)) [ti] (Just l) [la, li]

confLog :: [Text]
confLog = ["/conf/project/haskell/flagellum.vim", "/conf/project_after/all.vim"]

target :: [Text]
target = [
  "Diagnostics",
  "",
  "Main project",
  "",
  "name: " <> flag,
  "root: " <> toText (toFilePath root),
  "type: " <> hask,
  "tags cmd: ctags -R --languages=agda,idris -f /projects/flagellum/.tags.tmp /projects/flagellum/",
  "types: " <> idr,
  "main language: " <> hask,
  "languages: " <> ag <> ", " <> idr,
  "",
  "loaded config files:"
  ] <> confLog

diagSpec :: ProteomeTest ()
diagSpec = do
  setL @Env Env.mainProject main
  setL @Env Env.configLog confLog
  proDiag def
  content <- currentBufferContent
  target === content

test_diag :: UnitTest
test_diag =
  vars >>= testWithDef diagSpec
