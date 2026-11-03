{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.Flake (menuFlake) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Gum (Item (..), ok, warn)
import qualified Gum
import Nixon.Core
import Nixon.Json
import Nixon.Menu.System (nixosBuildDiff)
import Nixon.Nix
import System.Directory
import System.FilePath

menuFlake :: IO ()
menuFlake = do
  cwd <- getCurrentDirectory
  md <- askDef "flake dir" "path to a flake" (T.pack cwd)
  forM_ md $ \d -> do
    home <- getHomeDirectory
    let p = case T.unpack d of ('~' : r) -> home <> r; x -> x
    ex <- doesDirectoryExist p
    if not ex then warn "no such directory" >> pause else do
      dir <- canonicalizePath p
      hasFlake <- doesFileExist (dir </> "flake.nix")
      if hasFlake then flakeLoop dir else do
        warn ("no flake.nix in " <> T.pack dir)
        c <- confirm "Create one from the default template?"
        when c (withCurrentDirectory dir (void (runCmd ["nix", "flake", "init"])))
        pause

flakeLoop :: FilePath -> IO ()
flakeLoop dir = menuLoop ("Flake: " <> T.pack dir)
  [ opt "Pick an output…" (pickOutput dir)
  , opt "Dev shell (nix develop)" (inDir (void (runCmd ["nix", "develop"])))
  , opt "Build default" $ do
      r <- inDir (runCmd ["nix", "build"])
      when r (ok ("result → " <> T.pack dir <> "/result"))
      pause
  , opt "Run default" $ inDir (runCmd ["nix", "run"]) >> pause
  , opt "Check" $ inDir (runCmd ["nix", "flake", "check"]) >> pause
  , opt "Format (nix fmt)" $ inDir (runCmd ["nix", "fmt"]) >> pause
  , opt "Show output tree" $ inDir (pagedCmd ["nix", "flake", "show"])
  , opt "Update all inputs" $ inDir (runCmd ["nix", "flake", "update"]) >> pause
  , opt "Update one input" $ do
      ins <- flakeInputs dir
      mi <- Gum.pickOne "pick an input" (map Gum.plain ins)
      forM_ mi $ \i -> do
        let n = T.unpack (itemKey i)
        r <- inDir (runCmd ["nix", "flake", "update", n])
        unless r (void (inDir (runCmd ["nix", "flake", "lock", "--update-input", n])))
        pause
  , opt "Override an input…" override
  , opt "Templates (new project)" menuTemplates
  ]
  where
    inDir = withCurrentDirectory dir
    override = do
      ins <- flakeInputs dir
      mi <- Gum.pickOne "input to override" (map Gum.plain ins)
      forM_ mi $ \i -> do
        mv <- askNE ("override " <> itemKey i <> " with") "e.g. path:/home/me/nixpkgs  or  github:NixOS/nixpkgs/nixos-unstable"
        forM_ mv $ \v -> do
          mc <- Gum.choose "then run…" ["develop", "build", "run", "check"]
          forM_ mc $ \c -> do
            let sub = if c == "check" then ["flake", "check"] else [T.unpack c]
            void (inDir (runCmd (["nix"] ++ sub ++ ["--override-input", T.unpack (itemKey i), T.unpack v])))
            pause

-- | Flake outputs can number in the hundreds → fzf with a preview pane.
pickOutput :: FilePath -> IO ()
pickOutput dir = do
  outs <- Gum.spin "Evaluating flake outputs…" (flakeOutputs dir)
  if null outs then warn "no outputs found (does the flake evaluate?)" >> pause else loop outs
  where
    loop outs = do
      let items = [ Item (outAttr o) (padR 56 (outAttr o) <> " " <> outType o) (Just (outPreview o)) | o <- outs ]
      sel <- Gum.pickOne "flake outputs for this system · enter: actions · esc: back" items
      forM_ sel $ \it -> do
        forM_ (filter ((== itemKey it) . outAttr) outs) (outputActions dir)
        loop outs

outputActions :: FilePath -> Out -> IO ()
outputActions dir o = do
  let attr = outAttr o
      ref = T.pack dir <> "#" <> attr
      refS = T.unpack ref
      top = T.takeWhile (/= '.') attr
      host = T.drop (T.length "nixosConfigurations.") attr
      acts :: [Text]
      acts
        | top `elem` ["packages", "legacyPackages", "checks", "formatter", "defaultPackage"] =
            ["Build", "Run", "Show derivation", "Closure size", "Build log"]
        | top == "apps" = ["Run"]
        | top `elem` ["devShells", "devShell"] = ["Enter shell (nix develop)", "Build"]
        | top == "nixosConfigurations" =
            ["Build toplevel", "Build & diff vs running system", "Switch (nixos-rebuild)", "Open in repl"]
        | top == "templates" = ["New project from this template"]
        | otherwise = ["Evaluate (JSON)"]
  mc <- Gum.choose attr (acts ++ ["Back"])
  case mc of
    Just "Build" -> runCmd ["nix", "build", refS] >> pause
    Just "Run" -> do
      ma <- ask "arguments" "optional"
      forM_ ma $ \a -> runCmd (["nix", "run", refS, "--"] ++ words (T.unpack a)) >> pause
    Just "Show derivation" -> do
      r <- capture ["nix", "derivation", "show", refS]
      maybe (captureAll ["nix", "show-derivation", refS]) pure r >>= Gum.pager
    Just "Closure size" -> closureReport ref >>= either Gum.bad Gum.pager
    Just "Build log" -> pagedCmd ["nix", "log", refS]
    Just "Enter shell (nix develop)" -> void (runCmd ["nix", "develop", refS])
    Just "Build toplevel" ->
      runCmd ["nix", "build", dir <> "#nixosConfigurations." <> T.unpack host <> ".config.system.build.toplevel"] >> pause
    Just "Build & diff vs running system" -> nixosBuildDiff (Just (T.pack dir <> "#" <> host))
    Just "Switch (nixos-rebuild)" -> do
      c <- confirm ("sudo nixos-rebuild switch --flake " <> T.pack dir <> "#" <> host <> " ?")
      when c $ do
        s <- sudoPrefix
        runCmd (s ++ ["nixos-rebuild", "switch", "--flake", dir <> "#" <> T.unpack host]) >> pause
    Just "Open in repl" ->
      void (runCmd ["nix", "repl", "--expr", "(builtins.getFlake \"" <> dir <> "\")." <> T.unpack attr])
    Just "New project from this template" -> do
      md <- askDef "destination" "" "./my-project"
      forM_ md $ \d -> runCmd ["nix", "flake", "new", T.unpack d, "-t", refS] >> pause
    Just "Evaluate (JSON)" -> do
      r <- capture ["nix", "eval", "--json", refS]
      case r >>= parse of
        Just v  -> Gum.pager (pretty v)
        Nothing -> captureAll ["nix", "eval", refS] >>= Gum.pager
    _ -> pure ()

menuTemplates :: IO ()
menuTemplates = do
  msrc <- askDef "template flake" "" "templates"
  forM_ msrc $ \src -> do
    m <- capture ["nix", "flake", "show", "--json", T.unpack src]
    let ts = [ (k, fromMaybe' (key "description" v >>= strOf)) | Just j <- [m >>= parse], Just t <- [key "templates" j], (k, v) <- members t ]
    if null ts then warn ("no templates in '" <> src <> "'") >> pause else do
      msel <- Gum.pickOne ("templates from " <> src) [ Item k (padR 26 k <> " " <> d) Nothing | (k, d) <- ts ]
      forM_ msel $ \s -> do
        md <- askDef "destination" "" "./my-project"
        forM_ md $ \d -> runCmd ["nix", "flake", "new", T.unpack d, "-t", T.unpack src <> "#" <> T.unpack (itemKey s)] >> pause
  where fromMaybe' = maybe "" id
