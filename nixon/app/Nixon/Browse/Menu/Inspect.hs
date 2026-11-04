{-# LANGUAGE OverloadedStrings #-}
-- | UI for the nix-inspect style browser.
module Nixon.Browse.Menu.Inspect (menuInspect) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Gum (Item (..), bad)
import qualified Gum
import Nixon.Browse.Core
import Nixon.Browse.Inspect
import Nixon.Core

menuInspect :: IO ()
menuInspect = do
  mref <- askDef "flake" "path, flake ref, or  expr:<nix expression>" "."
  forM_ mref $ \ref -> do
    root <- rootExpr ref
    r <- withRepl root $ \repl -> go repl []
    either (\e -> bad e >> pause) pure r

-- | Show one level and loop on it. Returning means "go up"; at the root that
-- exits the browser. Re-evaluating a level after coming back is cheap because
-- the repl keeps what it has already forced.
go :: Repl -> [Text] -> IO ()
go repl path = do
  res <- Gum.spin ("Evaluating " <> breadcrumb path <> "…") (replInspect repl path)
  case res of
    Left e  -> bad e >> pause
    Right n -> case ndKind n of
      KLeaf -> Gum.pager (T.unlines [ breadcrumb path, ndType n, "", fromMaybe "" (ndValue n) ])
      _     -> pick n
  where
    pick n = do
      let rows = Item keyUp "..  (back)" Nothing
               : [ Item c c Nothing | c <- ndChildren n ]
          hdr  = "inspect " <> breadcrumb path <> "  [" <> ndType n <> kindTag (ndKind n)
                 <> ", " <> T.pack (show (length (ndChildren n))) <> " entries]"
                 <> " · enter: open · esc: up"
      sel <- Gum.pickOne hdr rows
      case sel of
        Just it | itemKey it /= keyUp -> go repl (path ++ [itemKey it]) >> pick n
        _                             -> pure ()

    kindTag KDrv = " derivation"
    kindTag _    = ""
