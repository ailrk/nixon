{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.Packages (menuPackages, menuHistory) where

import Control.Monad
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Gum (Item (..), bad, dim, ok, warn)
import qualified Gum
import Nixon.Core
import Nixon.Nix
import System.Directory (doesFileExist)

pkgItem :: Pkg -> Item
pkgItem p = Item (pkAttr p)
  (padR 34 (pkAttr p) <> " " <> padR 14 (pkVersion p) <> " " <> pkDesc p)
  (Just (T.unwords [pkPname p, pkVersion p] <> "\n\n" <> pkDesc p))

-- | Interactive search; returns the selected attribute names.
searchPackages :: IO (Maybe [Text])
searchPackages = do
  ch <- getChannel
  mq <- askNE "🔍 search" ("e.g. ripgrep   (channel: " <> ch <> ")")
  case mq of
    Nothing -> pure Nothing
    Just q -> do
      pkgs <- Gum.spin ("Searching " <> ch <> " for '" <> q <> "'…") (searchPkgs [q])
      if null pkgs
        then warn ("no results for '" <> q <> "'") >> pause >> pure Nothing
        else do
          -- result lists can be long → fzf (with preview pane) when available
          sel <- Gum.pickMany "tab: multi-select · enter: confirm" (map pkgItem pkgs)
          pure (if null sel then Nothing else Just (map itemKey sel))

-- | Version history from nixhub; yields a pinned flake installable.
oldVersionPick :: Text -> IO (Maybe Text)
oldVersionPick name0 = do
  hc <- have "curl"
  if not hc then bad "curl is required" >> pure Nothing else do
    let given = snd (T.breakOnEnd "#" name0)
    mname <- if T.null given then askNE "package" "attribute name, e.g. nodejs" else pure (Just given)
    case mname of
      Nothing -> pure Nothing
      Just name -> do
        vs <- nixhubVersions name
        if null vs
          then warn ("no version history for '" <> name <> "' on nixhub (or the service is unreachable)") >> pure Nothing
          else do
            mv <- Gum.pickOne ("versions of " <> name <> " (newest first)") (map Gum.plain vs)
            case mv of
              Nothing -> pure Nothing
              Just v -> do
                r <- nixhubResolve name (itemKey v)
                case r of
                  Nothing -> warn (name <> " " <> itemKey v <> " is not available for " <> curSystem) >> pure Nothing
                  Just ref -> ok ("pinned: " <> ref) >> pure (Just ref)

packageActions :: [Text] -> IO ()
packageActions [] = pure ()
packageActions picks = do
  refs <- mapM toRef picks
  let args = map T.unpack refs
      p0 = firstOr "" picks
      r0 = firstOr "" refs
  menuLoop ("Selected: " <> T.unwords picks)
    [ opt "Temporary shell" $ do
        warn ("entering a shell with: " <> T.unwords picks <> "  (exit to return)")
        void (runCmd (["nix", "shell"] ++ args))
    , opt "Run" $ do
        ma <- ask "arguments" ("for " <> p0 <> " (optional)")
        forM_ ma $ \a -> runCmd (["nix", "run", T.unpack r0, "--"] ++ words (T.unpack a)) >> pause
    , opt "Install to profile" $ do
        c <- confirm ("Install " <> T.unwords picks <> " to your profile?")
        when c $ do
          r <- runCmd (["nix", "profile", "install"] ++ args)
          when r (ok "installed")
          pause
    , opt "Show info" $ mapM packageInfo picks >>= Gum.pager . T.intercalate "\n"
    , opt "Closure size" $ closureReport r0 >>= either bad Gum.pager
    , opt "Is it in the binary cache?" $ cachedReport r0 >> pause
    , ("Find an older version", do
        r <- oldVersionPick p0
        case r of
          Just ref -> packageActions [ref] >> pure Leave
          Nothing  -> pause >> pure Stay)
    , ("Search again", do
        r <- searchPackages
        forM_ r packageActions
        pure Leave)
    ]

menuLocate :: IO ()
menuLocate = do
  db <- locateDb
  ex <- doesFileExist db
  ready <- if ex then pure True else do
    warn "no nix-index database found"
    c <- confirm "Download the prebuilt database (~50 MB)?"
    if c then locateUpdate else pure False
  if not ready then pause else do
    mpat <- askNE "file" "e.g. bin/rg, libssl.so, pkg-config"
    forM_ mpat $ \pat -> do
      mmode <- Gum.choose "match" ["exact file name", "regex on full path"]
      forM_ mmode $ \m -> do
        rs <- locateLines (if "exact" `T.isPrefixOf` m then Exact else Regex) pat
        if null rs
          then warn ("nothing provides '" <> pat <> "'") >> pause
          else do
            sel <- Gum.pickMany "which package? (tab: multi-select)"
                     [ Item a (padR 40 a <> " " <> p) Nothing | (a, p) <- rs ]
            let pks = nub (sort (map (stripOutput . itemKey) sel))
            unless (null pks) (packageActions pks)

menuPackages :: IO ()
menuPackages = menuLoop "Packages"
  [ optLeave "Search nixpkgs" (searchPackages >>= mapM_ packageActions)
  , optLeave "Find a package by file (nix-locate)" menuLocate
  , optLeave "Find an older version (nixhub)" (oldVersionPick "" >>= mapM_ (packageActions . pure))
  , optLeave "Update the file index" (void locateUpdate >> pause)
  ]

menuHistory :: IO ()
menuHistory = menuLoop "History"
  [ opt "Command history" $ historyLines >>= Gum.pager . T.unlines
  , opt "Re-run a command" $ do
      hs <- historyEntries
      sel <- Gum.pickOne "pick a command to run again" [ Item c (d <> "  " <> c) Nothing | (d, c) <- hs ]
      forM_ sel $ \it -> do
        dim ("$ " <> itemKey it)
        go <- confirm "Run it?"
        when go $ do
          record (itemKey it)
          void (spawn ["bash", "-c", T.unpack (itemKey it)])
          pause
  , opt "Clear history" $ do
      c <- confirm "Clear command history?"
      when c clearHistory
  ]
