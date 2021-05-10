module Proteome.Grep.Line where

import Conduit (ConduitT, await, mapC, yield, (.|))
import qualified Data.Set as Set (difference, empty, fromList, toList, union)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))

import Proteome.Data.GrepOutputLine (GrepOutputLine(GrepOutputLine))

newtype PerLine =
  PerLine { unPerLine :: MenuItem GrepOutputLine }
  deriving (Show)

instance Eq PerLine where
  PerLine (MenuItem (GrepOutputLine p1 l1 _ _) _ _) == PerLine (MenuItem (GrepOutputLine p2 l2 _ _) _ _) =
    p1 == p2 && l1 == l2

instance Ord PerLine where
  (PerLine (MenuItem (GrepOutputLine p1 l1 _ _) _ _)) <= (PerLine (MenuItem (GrepOutputLine p2 l2 _ _) _ _)) =
    (p1, l1) <= (p2, l2)

unique ::
  Ord a =>
  Monad m =>
  ConduitT [a] [a] m ()
unique =
  consume Set.empty
  where
    consume seen =
      traverse_ (spin seen) =<< await
    spin seen as =
      yield (Set.toList uniques) *>
      consume (Set.union uniques seen)
      where
        uniques =
          Set.difference (Set.fromList as) seen

uniqueGrepLines ::
  Monad m =>
  ConduitT [MenuItem GrepOutputLine] [MenuItem GrepOutputLine] m ()
uniqueGrepLines =
  mapC (fmap PerLine) .| unique .| mapC (fmap unPerLine)
