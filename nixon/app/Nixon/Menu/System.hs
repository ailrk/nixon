{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.System (menuNixos, menuHomeManager, menuDarwin, nixosBuildDiff) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Gum (warn)
import qualified Gum
import Nixon.Core
import Nixon.Menu.Inspect (diffClosures, pickGenerations)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)

flakeArgs :: Text -> [String]
flakeArgs f = concat [ ["--flake", T.unpack f] | not (T.null f) ]

-- | Build a NixOS config, show the closure diff vs. the running system, optionally switch.
nixosBuildDiff :: Maybe Text -> IO ()
nixosBuildDiff preset = do
  mflake <- maybe (ask "flake ref" "e.g. /etc/nixos#host  (blank = channel config)") (pure . Just) preset
  forM_ mflake $ \flake -> do
    mout <-
      if not (T.null flake)
        then do
          let (base, h) = T.breakOn "#" flake
          host <- if T.null h then maybe "" T.strip <$> capture ["hostname"] else pure (T.drop 1 h)
          let target = T.unpack base <> "#nixosConfigurations." <> T.unpack host <> ".config.system.build.toplevel"
          record ("nix build --no-link --print-out-paths " <> T.pack target)
          fmap T.strip <$> captureOut ["nix", "build", "--no-link", "--print-out-paths", target]
        else withSystemTempDirectory "nixon" $ \tmp -> do
          r <- withCurrentDirectory tmp (runCmd ["nixos-rebuild", "build"])
          if r then Just . T.pack <$> canonicalizePath (tmp </> "result") else pure Nothing
    case mout of
      Nothing -> pause
      Just out -> do
        diff <- captureAll ["nix", "store", "diff-closures", "/run/current-system", T.unpack out]
        Gum.pager ("changes vs. the running system:\n\n" <> diff)
        go <- confirm "Activate it now with 'nixos-rebuild switch'?"
        when go $ do
          s <- sudoPrefix
          void (runCmd (s ++ ["nixos-rebuild", "switch"] ++ flakeArgs flake))
        pause

specialisations :: IO ()
specialisations = do
  ex <- doesDirectoryExist "/run/current-system/specialisation"
  list <- if ex then listDirectory "/run/current-system/specialisation" else pure []
  mc <- Gum.choose "Specialisations" ["Activate one", "Return to the base configuration"]
  s <- sudoPrefix
  case mc of
    Just "Activate one"
      | null list -> warn "no specialisations in the running system"
      | otherwise -> do
          ms <- Gum.choose "specialisation" (map T.pack list)
          forM_ ms $ \x -> runCmd (s ++ ["/run/current-system/specialisation/" <> T.unpack x <> "/bin/switch-to-configuration", "test"])
    Just _ -> void (runCmd (s ++ ["/nix/var/nix/profiles/system/bin/switch-to-configuration", "test"]))
    Nothing -> pure ()
  pause

menuNixos :: IO ()
menuNixos = do
  ok' <- have "nixos-rebuild"
  if not ok' then warn "this doesn't look like NixOS" >> pause else
    menuLoop "NixOS" $
      [ opt a (rebuild a) | a <- ["switch", "test", "boot", "build", "dry-activate"] ] ++
      [ opt "Build & diff vs running system" (nixosBuildDiff Nothing)
      , opt "List generations" $ do
          o <- capture ["nixos-rebuild", "list-generations"]
          maybe (pagedCmd ["nix-env", "-p", "/nix/var/nix/profiles/system", "--list-generations"]) Gum.pager o
      , opt "Diff two generations" $ pickGenerations >>= maybe pause (\(a, b) -> diffClosures a b)
      , opt "Specialisations" specialisations
      , opt "Rollback" $ do
          c <- confirm "Switch to the previous system generation?"
          when c $ do
            s <- sudoPrefix
            runCmd (s ++ ["nixos-rebuild", "switch", "--rollback"]) >> pause
      ]
  where
    rebuild a = do
      mf <- ask "flake ref" "e.g. /etc/nixos#host  (blank = channel config)"
      forM_ mf $ \f -> do
        let args = T.unpack a : flakeArgs f
        c <- confirm ("Run: sudo nixos-rebuild " <> T.pack (unwords args) <> " ?")
        when c $ do
          s <- sudoPrefix
          runCmd (s ++ ["nixos-rebuild"] ++ args) >> pause

menuHomeManager :: IO ()
menuHomeManager = do
  ok' <- have "home-manager"
  if not ok' then warn "home-manager is not installed" >> pause else
    menuLoop "Home Manager" $
      [ opt a $ do
          mf <- ask "flake ref" "e.g. ~/dotfiles#me  (blank = ~/.config/home-manager)"
          forM_ mf $ \f -> runCmd (["home-manager", T.unpack a] ++ flakeArgs f) >> pause
      | a <- ["switch", "build"] ] ++
      [ opt "Generations" $ pagedCmd ["home-manager", "generations"]
      , opt "Diff two generations" $ pickGenerations >>= maybe pause (\(a, b) -> diffClosures a b)
      , opt "Expire generations" $ do
          ma <- Gum.choose "Expire generations older than…" ["-7 days", "-14 days", "-30 days", "-90 days"]
          forM_ ma $ \a -> do
            c <- confirm ("Expire home-manager generations older than " <> T.drop 1 a <> "?")
            when c (runCmd ["home-manager", "expire-generations", T.unpack a] >> pause)
      , opt "Installed packages" $ pagedCmd ["home-manager", "packages"]
      , opt "News" $ pagedCmd ["home-manager", "news"]
      ]

menuDarwin :: IO ()
menuDarwin = do
  ok' <- have "darwin-rebuild"
  if not ok' then warn "darwin-rebuild is not installed" >> pause else
    menuLoop "nix-darwin" $
      [ opt a $ do
          mf <- ask "flake ref" "e.g. ~/dotfiles#mac  (blank = default config)"
          forM_ mf $ \f -> do
            s <- if a == "switch" then sudoPrefix else pure []
            runCmd (s ++ ["darwin-rebuild", T.unpack a] ++ flakeArgs f) >> pause
      | a <- ["switch", "build", "check"] ] ++
      [ opt "List generations" $ pagedCmd ["darwin-rebuild", "--list-generations"]
      , opt "Rollback" $ do
          c <- confirm "Roll back nix-darwin?"
          when c $ do
            s <- sudoPrefix
            runCmd (s ++ ["darwin-rebuild", "switch", "--rollback"]) >> pause
      ]
