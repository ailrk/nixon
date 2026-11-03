{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.Admin (menuChannels, menuMaintenance, menuCaches) where

import Control.Exception
import Control.Monad
import Data.List (sortOn)
import Data.Maybe
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Gum (Item (..), ok, warn)
import qualified Gum
import Nixon.Core
import Nixon.Nix
import System.Directory

-- channels & registry ------------------------------------------------------------

menuChannels :: IO ()
menuChannels = menuLoop "Channels & registry"
  [ opt "Pick a nixpkgs branch (live from GitHub)" $ do
      g <- have "git"
      if not g then warn "git is required" >> pause else do
        out <- Gum.spin "Fetching branches…" $ capture
          ["git", "ls-remote", "--heads", "https://github.com/NixOS/nixpkgs", "refs/heads/nixos-*", "refs/heads/nixpkgs-*"]
        let bs = sortOn Down [ fromMaybe r (T.stripPrefix "refs/heads/" r) | l <- T.lines (fromMaybe "" out), (_ : r : _) <- [T.words l] ]
        sel <- Gum.pickOne "nixpkgs branch" (map Gum.plain bs)
        forM_ sel (setChannel . ("github:NixOS/nixpkgs/" <>) . itemKey)
        pause
  , opt "Use the registry's nixpkgs (default)" $ setChannel "nixpkgs" >> pause
  , opt "Use a custom ref…" $ do
      mv <- askNE "flake ref" "e.g. github:NixOS/nixpkgs/nixos-unstable"
      forM_ mv setChannel
      pause
  , opt "Pin registry nixpkgs to its current revision" $ runCmd ["nix", "registry", "pin", "nixpkgs"] >> pause
  , opt "Point the registry's nixpkgs at nixonchannel" $ do
      ch <- getChannel
      runCmd ["nix", "registry", "add", "nixpkgs", T.unpack ch] >> pause
  , opt "List registry" $ pagedCmd ["nix", "registry", "list"]
  , opt "Add registry entry" $ do
      mi <- askNE "id" "e.g. nixpkgs"
      mv <- maybe (pure Nothing) (const (askNE "points to" "e.g. github:NixOS/nixpkgs/nixos-unstable")) mi
      forM_ ((,) <$> mi <*> mv) $ \(i, v) -> runCmd ["nix", "registry", "add", T.unpack i, T.unpack v]
      pause
  , opt "Remove registry entry" $ do
      out <- fromMaybe "" <$> capture ["nix", "registry", "list"]
      let ids = [ fromMaybe i (T.stripPrefix "flake:" i) | l <- T.lines out, ("user" : i : _) <- [T.words l] ]
      mi <- Gum.pickOne "user registry entries" (map Gum.plain ids)
      forM_ mi $ \i -> do
        c <- confirm ("Remove '" <> itemKey i <> "' from the user registry?")
        when c (void (runCmd ["nix", "registry", "remove", T.unpack (itemKey i)]))
      pause
  , opt "Prefetch a flake" $ do
      mv <- askNE "flake ref" "e.g. github:NixOS/nixpkgs/nixos-unstable"
      forM_ mv $ \v -> runCmd ["nix", "flake", "prefetch", T.unpack v]
      pause
  ]

-- maintenance & GC ------------------------------------------------------------

gcOlderThan :: IO ()
gcOlderThan = do
  ms <- Gum.choose "Which profiles?" ["Mine (no sudo)", "System + all users (sudo)"]
  forM_ ms $ \scope -> do
    ma <- Gum.choose "Delete generations older than…" ["1d", "7d", "14d", "30d", "90d", "custom…"]
    age <- case ma of
      Just "custom…" -> askNE "age" "e.g. 21d"
      other          -> pure other
    forM_ age $ \a -> do
      c <- confirm ("Delete generations older than " <> a <> " (" <> scope <> ") and collect garbage?")
      when c $ do
        s <- if "Mine" `T.isPrefixOf` scope then pure [] else sudoPrefix
        runCmd (s ++ ["nix-collect-garbage", "--delete-older-than", T.unpack a])
        pause

menuGcRoots :: IO ()
menuGcRoots = do
  rs <- Gum.spin "Finding GC roots…" rootsLines
  let items = [ Item r l Nothing | ((_, r, _), l) <- zip rs (roots2text rs) ]
  sel <- Gum.pickMany "what's keeping store paths alive (biggest first) · tab: select to delete" items
  dels <- fmap catMaybes . forM (map itemKey sel) $ \r -> do
    isL <- pathIsSymbolicLink (T.unpack r) `catch` \(_ :: IOException) -> pure False
    if isL && not (any (`T.isPrefixOf` r) ["/nix/var/nix/profiles/", "/run/", "/proc/"])
      then pure (Just r)
      else warn ("skipping '" <> r <> "' (not a removable symlink root — profiles and system roots are managed by nix)") >> pure Nothing
  unless (null dels) $ do
    mapM_ (TIO.putStrLn . ("  " <>)) dels
    c <- confirm ("Delete these " <> T.pack (show (length dels)) <> " root symlink(s)? The store paths become collectable.")
    when c $ do
      r <- runCmd ("rm" : "--" : map T.unpack dels)
      when r (ok "removed — run a garbage collection to reclaim space")
  pause

menuMaintenance :: IO ()
menuMaintenance = menuLoop "Maintenance & garbage collection"
  [ opt "Store size" $ do
      o <- Gum.spin "Measuring /nix/store…" (capture ["du", "-sh", "/nix/store"])
      TIO.putStr (fromMaybe "(du failed)\n" o)
      pause
  , opt "GC dry run" $ pagedCmd ["nix-collect-garbage", "--dry-run"] >> pause
  , opt "Garbage collect" $ do
      c <- confirm "Delete unreachable store paths?"
      when c (runCmd ["nix-collect-garbage"] >> pause)
  , opt "GC + delete old generations (all)" $ do
      c <- confirm "Delete ALL old generations and unreachable paths? This cannot be undone."
      when c (runCmd ["nix-collect-garbage", "-d"] >> pause)
  , opt "GC + delete generations older than…" gcOlderThan
  , opt "Wipe profile history older than…" $ do
      ma <- Gum.choose "Older than…" ["7d", "14d", "30d", "90d"]
      forM_ ma $ \a -> runCmd ["nix", "profile", "wipe-history", "--older-than", T.unpack a] >> pause
  , opt "GC roots browser" menuGcRoots
  , opt "Optimise store (dedupe)" $ runCmd ["nix", "store", "optimise"] >> pause
  ]

-- caches & remote ------------------------------------------------------------

menuCaches :: IO ()
menuCaches = menuLoop "Caches & remote builders"
  [ opt "Substituter status" $ substituterReport >>= Gum.pager
  , opt "Is a package cached?" $ do
      ch <- getChannel
      mr <- askDef "installable" "e.g. nixpkgs#hello" (ch <> "#")
      forM_ (mfilter (not . T.null) mr) cachedReport
      pause
  , opt "nix copy to/from another store" $ do
      md <- Gum.choose "direction" ["to", "from"]
      forM_ md $ \d -> do
        mu <- askNE "store URI" "e.g. ssh-ng://user@host or file:///mnt/usb"
        mr <- maybe (pure Nothing) (const (askNE "installable / path" "e.g. nixpkgs#hello or /run/current-system")) mu
        forM_ ((,) <$> mu <*> mr) $ \(u, r) -> do
          skip <- confirm "Skip signature checks? (needed for unsigned local builds)"
          void (runCmd (["nix", "copy", "--" <> T.unpack d, T.unpack u] ++ ["--no-check-sigs" | skip] ++ [T.unpack r]))
        pause
  , opt "Cachix: push a build" $ do
      mn <- askNE "cachix cache" "name of your cache"
      mr <- maybe (pure Nothing) (const (askNE "installable" "e.g. .#default")) mn
      forM_ ((,) <$> mn <*> mr) $ \(n, r) -> do
        mp <- captureOut ["nix", "build", "--no-link", "--print-out-paths", T.unpack r]
        forM_ mp $ \paths -> do
          record ("nix build --no-link --print-out-paths " <> r <> " | cachix push " <> n)
          cmd <- toolCmd "cachix" "cachix" ["push", T.unpack n]
          void (spawnWithInput cmd paths)
      pause
  , opt "Cachix: use a cache" $ do
      mn <- askNE "cachix cache" "e.g. nix-community"
      forM_ mn $ \n -> do
        record ("cachix use " <> n)
        toolCmd "cachix" "cachix" ["use", T.unpack n] >>= void . spawn
      pause
  , opt "Remote builders" $ do
      b <- nixConf "builders"
      ex <- doesFileExist "/etc/nix/machines"
      ms <- if ex then TIO.readFile "/etc/nix/machines" else pure ""
      Gum.pager ("builders = " <> b <> "\n\n" <> (if ex then "/etc/nix/machines:\n" <> ms else ""))
  , opt "Test a builder connection" $ do
      mu <- askNE "builder store URI" "e.g. ssh-ng://user@builder"
      forM_ mu $ \u -> do
        r <- runCmd ["nix", "store", "info", "--store", T.unpack u]
        unless r (void (runCmd ["nix", "store", "ping", "--store", T.unpack u]))
      pause
  ]
