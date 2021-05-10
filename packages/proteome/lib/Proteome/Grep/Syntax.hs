module Proteome.Grep.Syntax where

import qualified Data.Map.Strict as Map (fromList)
import Ribosome.Data.Syntax (
  HiLink(HiLink),
  Syntax(Syntax),
  SyntaxItem(siOptions, siParams),
  syntaxMatch,
  syntaxVerbatim,
  )
import Text.RawString.QQ (r)

lineNumber :: Text
lineNumber =
  "\57505"

path :: SyntaxItem
path =
  item { siParams = params }
  where
    item = syntaxMatch "ProGrepPath" ([r|^.*\ze|] <> lineNumber)
    params = Map.fromList [("nextgroup", "ProGrepLN")]

ln :: SyntaxItem
ln =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProGrepLN" lineNumber
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "ProGrepLine")]

line :: SyntaxItem
line =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProGrepLine" [r|\d\+\ze:|]
    options = ["contained"]
    params = Map.fromList [("nextgroup", "ProGrepColon")]

colon :: SyntaxItem
colon =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProGrepColon" [r|:|]
    options = ["contained"]
    params = Map.fromList [("nextgroup", "ProGrepCol")]

col :: SyntaxItem
col =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProGrepCol" [r|\d\+|]
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "ProGrepText")]

text_ :: SyntaxItem
text_ =
  item { siOptions = options }
  where
    item = syntaxMatch "ProGrepText" [r|.\+|]
    options = ["contained"]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlPath :: HiLink
hlPath =
  HiLink "ProGrepPath" "Type"

hlLn :: HiLink
hlLn =
  HiLink "ProGrepLN" "LineNr"

hlLine :: HiLink
hlLine =
  HiLink "ProGrepLine" "Constant"

hlCol :: HiLink
hlCol =
  HiLink "ProGrepCol" "Constant"

hlText :: HiLink
hlText =
  HiLink "ProGrepText" "Directory"

grepSyntax :: Syntax
grepSyntax =
  Syntax items [] links
  where
    items =
      [path, ln, line, col, colon, text_, sync]
    links =
      [hlPath, hlLn, hlLine, hlCol, hlText]
