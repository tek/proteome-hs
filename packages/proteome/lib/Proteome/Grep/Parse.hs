module Proteome.Grep.Parse where

import Chiasma.Data.Ident (generateIdent, identText)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as Text (strip, stripPrefix)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import Text.Parser.Char (anyChar, char, noneOf)
import Text.Parser.Combinators (manyTill)
import Text.Parser.Token (TokenParsing, natural)

import Proteome.Data.GrepOutputLine (GrepOutputLine(GrepOutputLine))
import Proteome.Grep.Syntax (lineNumber)

grepParser ::
  TokenParsing m =>
  m GrepOutputLine
grepParser =
  GrepOutputLine <$> path <*> (subtract 1 <$> number) <*> optional number <*> (toText <$> many anyChar)
  where
    path =
      toText <$> manyTill (noneOf ":") (char ':')
    number =
      (fromInteger <$> natural) <* char ':'

formatGrepLine :: Text -> GrepOutputLine -> Text
formatGrepLine cwd (GrepOutputLine path line col text') =
  relativePath <> " " <> lineNumber <> " " <> show line <> ":" <> show (fromMaybe 1 col) <> " " <> Text.strip text'
  where
    relativePath =
      fromMaybe path (Text.stripPrefix (cwd <> "/") path)

parseGrepOutput ::
  MonadRibo m =>
  Text ->
  Text ->
  m (Maybe (MenuItem GrepOutputLine))
parseGrepOutput cwd =
  item . parseOnly grepParser
  where
    item (Right a) = do
      ident <- identText <$> generateIdent
      return (Just (convert ident a))
    item (Left err) =
      Nothing <$ logDebug ("parsing grep output failed: " <> err)
    convert _ file =
      MenuItem file text' text'
      where
        text' =
          formatGrepLine cwd file
