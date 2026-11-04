{-# LANGUAGE OverloadedStrings #-}
-- | UI for the nix-tree style closure browser.
module Nixon.Browse.Menu.Tree (menuTree) where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Gum (Item (..), bad)
import qualified Gum
import Nixon.Browse.Core
import Nixon.Browse.Tree
import Nixon.Core
import Nixon.Nix (human, resolveTarget)

menuTree :: IO ()
menuTree = do
  mt <- askDef "target" "installable or store path" "/run/current-system"
  forM_ mt $ \t -> do
    ps <- Gum.spin ("Resolving " <> t <> "…") (resolveTarget t)
    mg <- spinForce "Reading closure…" (maybe 0 (M.size . gNodes)) (loadGraph ps)
    case (mg, gRoots <$> mg) of
      (Just g, Just (r : _)) -> browse g r Deps
      _                      -> bad ("could not read the closure of '" <> t <> "'") >> pause

-- | Show one store path and loop on it. Returning means "go up"; at the root
-- that exits the browser. The view (dependencies / referrers) is inherited by
-- children and can be toggled per level.
browse :: Graph -> Text -> View -> IO ()
browse g cur view = case nodeOf g cur of
  Nothing -> bad ("not in the closure: " <> cur) >> pause
  Just sp -> do
    let ns   = neighbours g view cur
        rows = [ upItem
               , Item keyToggle "⇄ switch dependencies / referrers" Nothing
               , Item keyWhy "? why does the root depend on this" Nothing ]
               ++ [ row s | s <- ns ]
        row s = Item (spPath s)
                  (padL 8 (human (spClosure s)) <> padL 9 (human (spNar s)) <> "  " <> storeShort (spPath s))
                  Nothing
        hdr = storeShort cur <> "  closure " <> human (spClosure sp) <> " · nar " <> human (spNar sp)
              <> " · " <> (if view == Deps then "dependencies" else "referrers")
              <> " (" <> T.pack (show (length ns)) <> ")"
              <> " · enter: open · esc: up"
    sel <- Gum.pickOne hdr rows
    forM_ sel $ \it ->
      let k = itemKey it in
      if      k == keyUp     then pure ()
      else if k == keyToggle then browse g cur (toggle view)
      else if k == keyWhy    then do
        forM_ (take 1 (gRoots g)) $ \root -> runPaged (whyCmd root cur)
        browse g cur view
      else browse g k view >> browse g cur view
