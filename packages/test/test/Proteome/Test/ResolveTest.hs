module Proteome.Test.ResolveTest where

import qualified Data.Map as Map (fromList)
import Hedgehog ((/==), (===))
import Path (absdir, parseAbsDir)
import Ribosome.File (canonicalPaths)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Unit (fixture)
import Test.Tasty (TestTree, testGroup)

import Proteome.Config (defaultTypeMarkers)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType)
import Proteome.Project.Resolve (resolveProject)
import Proteome.Test.Project (fn, l, la, li, ta, ti, tp)
import Proteome.Test.Unit (ProteomeTest, testDef)

paths :: [FilePath]
paths = [
  "~/../test/dir",
  "~"
  ]

test_canonicalPaths :: UnitTest
test_canonicalPaths = do
  canon <- canonicalPaths paths
  canon /== paths

root :: ProjectRoot
root = ProjectRoot [absdir|/projects/haskell/flagellum|]

typeMap :: Map ProjectType [ProjectType]
typeMap = Map.fromList [(tp, [ti, ta])]

config :: ProjectConfig
config =
  ProjectConfig [[absdir|/projects|]] def def typeMap def (Map.fromList [(tp, l)]) (Map.fromList [(l, [li, la])])

targetProject :: Project
targetProject =
  Project (DirProject fn root (Just tp)) [ti, ta] (Just l) [li, la]

typeMapTest :: ProteomeTest ()
typeMapTest = do
  project <- resolveProject [] config (Just root) fn (Just tp)
  targetProject === project

test_typeMap :: UnitTest
test_typeMap =
  testDef typeMapTest

markerConfig :: ProjectConfig
markerConfig = ProjectConfig def def def def defaultTypeMarkers def def

markerTarget :: ProjectRoot -> Project
markerTarget root' =
  Project (DirProject fn root' (Just tp)) [] (Just l) []

markerTest :: ProteomeTest ()
markerTest = do
  dir <- parseAbsDir =<< fixture "projects/haskell/flagellum"
  let root' = ProjectRoot dir
  project <- resolveProject [] markerConfig (Just root') fn Nothing
  markerTarget root' === project

test_marker :: UnitTest
test_marker =
  testDef markerTest

test_resolve :: TestTree
test_resolve =
  testGroup "project resolution" [
    unitTest "canonicalize paths" test_canonicalPaths,
    unitTest "project type map" test_typeMap,
    unitTest "root marker" test_marker
  ]
