{-# LANGUAGE OverloadedStrings #-}
-- | Pick from a (possibly huge) list: fzf when installed, Brick filter otherwise.
module Gum.Pick (pickWith, pickOne, pickMany, pickOrType) where

import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Gum.Fzf
import Gum.List

-- | Empty result means cancelled / nothing chosen.
pickWith :: Bool -> Text -> [Item] -> IO [Item]
pickWith multi hdr items = do
  fz <- fzfAvailable
  if fz
    then maybe [] snd <$> fzfPick (FzfOpts hdr multi) items
    else do
      let labels = map itemLabel items
      r <- if multi then filterMany Nothing hdr labels
                    else fmap (fmap pure) (filterOne hdr labels)
      pure (concatMap back (maybe [] id r))
  where
    back l = take 1 (filter ((== l) . itemLabel) items)

pickOne :: Text -> [Item] -> IO (Maybe Item)
pickOne hdr items = listToMaybe <$> pickWith False hdr items

pickMany :: Text -> [Item] -> IO [Item]
pickMany = pickWith True

-- | returns the selected item's key, or whatever was typed when nothing
-- matched.
pickOrType :: Text -> [Item] -> IO (Maybe Text)
pickOrType hdr items = do
  fz <- fzfAvailable
  if fz
    then do
      r <- fzfPick (FzfOpts hdr False) items
      pure $ case r of
        Nothing                       -> Nothing
        Just (_, it : _)              -> Just (itemKey it)
        Just (q, []) | T.null q       -> Nothing
                     | otherwise      -> Just q
    else do
      r <- filterFree hdr (map itemLabel items)
      pure (fmap (\l -> maybe l itemKey (find ((== l) . itemLabel) items)) r)
