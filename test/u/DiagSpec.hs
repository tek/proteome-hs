{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (Default(def))
import Test.Framework
import Ribosome.Api.Buffer (currentBufferContent)
import Proteome.Data.Proteome
import Proteome.Test.Unit (specWithDef)
import Proteome.Diag (proDiag)
import Config (vars)

target :: [String]
target = [
  "Diagnostics",
  "",
  "Main project:",
  "name: main",
  "types: ",
  "main language: none",
  "languages: "
  ]

diagSpec :: Proteome ()
diagSpec = do
  proDiag def
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  vars >>= specWithDef diagSpec
