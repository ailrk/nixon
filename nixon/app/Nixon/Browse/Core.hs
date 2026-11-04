{-# LANGUAGE OverloadedStrings #-}
module Nixon.Browse.Core
  ( -- loading
    spinForce
  , absolutize
    -- display
  , breadcrumb
  , storeShort
    -- picker entries that are not data
  , keyUp, keyToggle, keyWhy
  , upItem
  ) where

import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Gum
import Gum (Item (..))
import System.Directory (canonicalizePath)

-- | Run an action under a spinner and force a cheap summary of its result
-- (a size or count) before the spinner stops, so lazy parsing happens while
-- the spinner is visible rather than after the picker has opened.
spinForce :: Text -> (a -> Int) -> IO a -> IO a
spinForce msg measure act = Gum.spin msg $ do
  x <- act
  _ <- evaluate (measure x)
  pure x

-- | Local paths become absolute (getFlake and path-info want that); flake
-- refs and anything else are left alone.
absolutize :: Text -> IO Text
absolutize t
  | any (`T.isPrefixOf` t) [".", "/"] = T.pack <$> canonicalizePath (T.unpack t)
  | otherwise                         = pure t

-- | "a.b.c" for a path, "." for the root.
breadcrumb :: [Text] -> Text
breadcrumb [] = "."
breadcrumb p  = T.intercalate "." p

-- | Drop the "/nix/store/<hash>-" prefix. Works with or without the
-- /nix/store/ prefix; anything that does not look like a store path is
-- returned unchanged.
storeShort :: Text -> Text
storeShort p
  | T.length b > 33 && T.index b 32 == '-' = T.drop 33 b
  | otherwise                              = p
  where b = fromMaybe p (T.stripPrefix "/nix/store/" p)

-- | Keys of picker rows that are actions, not data. Control characters so they
-- cannot collide with an attribute name or a store path.
keyUp, keyToggle, keyWhy :: Text
keyUp     = "\SOHup"
keyToggle = "\SOHtoggle"
keyWhy    = "\SOHwhy"

upItem :: Item
upItem = Item keyUp "↑ back" Nothing
