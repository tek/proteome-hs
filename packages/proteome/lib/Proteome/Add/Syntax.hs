module Proteome.Add.Syntax where

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
    item = syntaxMatch "ProAddAsterisk" [r|^ \*|]
    options = ["skipwhite"]
    params = Map.fromList [("nextgroup", "ProAddName")]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlAsterisk :: HiLink
hlAsterisk =
  HiLink "ProAddAsterisk" "Todo"

hlName :: HiLink
hlName =
  HiLink "ProAddName" "Type"

addSyntax :: Syntax
addSyntax =
  Syntax items [] links
  where
    items =
      [asterisk, sync]
    links =
      [hlAsterisk, hlName]
