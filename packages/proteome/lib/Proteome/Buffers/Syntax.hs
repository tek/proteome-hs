module Proteome.Buffers.Syntax where

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
    item = syntaxMatch "ProBuffersAsterisk" [r|^ \*|]
    options = ["skipwhite"]
    params = Map.fromList [("nextgroup", "ProBuffersNumber")]

number :: SyntaxItem
number =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProBuffersNumber" [r|\d\+|]
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "ProBuffersName")]

line :: SyntaxItem
line =
  item { siOptions = options }
  where
    item = syntaxMatch "ProBuffersName" ".*$"
    options = ["contained"]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlAsterisk :: HiLink
hlAsterisk =
  HiLink "ProBuffersAsterisk" "Todo"

hlNumber :: HiLink
hlNumber =
  HiLink "ProBuffersNumber" "Directory"

hlName :: HiLink
hlName =
  HiLink "ProBuffersName" "Type"

buffersSyntax :: Syntax
buffersSyntax =
  Syntax items [] links
  where
    items =
      [asterisk, number, line, sync]
    links =
      [hlAsterisk, hlNumber, hlName]
