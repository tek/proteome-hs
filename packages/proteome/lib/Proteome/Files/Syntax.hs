module Proteome.Files.Syntax where

import qualified Data.Map.Strict as Map (fromList)
import Ribosome.Data.Syntax (
  HiLink(HiLink),
  Syntax(Syntax),
  SyntaxItem(siOptions, siParams),
  syntaxMatch,
  syntaxVerbatim,
  )
import Text.RawString.QQ (r)

asterisk :: SyntaxItem
asterisk =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProFilesAsterisk" [r|^ \*|]
    options = ["skipwhite"]
    params = Map.fromList [("nextgroup", "ProFilesBase,ProFilesName")]

base :: SyntaxItem
base =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProFilesBase" [r|\[[^]]\+\]|]
    options = ["skipwhite", "contained"]
    params = Map.fromList [("nextgroup", "ProFilesName")]

name :: SyntaxItem
name =
  item { siOptions = options }
  where
    item = syntaxMatch "ProFilesName" ".*"
    options = ["contained"]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlAsterisk :: HiLink
hlAsterisk =
  HiLink "ProFilesAsterisk" "Todo"

hlBase :: HiLink
hlBase =
  HiLink "ProFilesBase" "Directory"

hlName :: HiLink
hlName =
  HiLink "ProFilesName" "Type"

filesSyntax :: Syntax
filesSyntax =
  Syntax items [] links
  where
    items =
      [asterisk, base, name, sync]
    links =
      [hlAsterisk, hlBase, hlName]
