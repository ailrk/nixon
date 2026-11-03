{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.Inspect
  ( menuInspect, menuDiff, pickGenerations, diffClosures) where

import Control.Exception
import Control.Monad
import Data.List (sortOn)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Gum (bad, dim, warn)
import qualified Gum
import Nixon.Core
import Nixon.Nix
import System.Directory
import System.FilePath
import Text.Read (readMaybe)
import Nixon.Json (pretty, parse)

-- generations & diffs ------------------------------------------------------------

userProfileBase :: IO (Maybe FilePath)
userProfileBase = do
  home <- getHomeDirectory
  r <- try (getSymbolicLinkTarget (home </> ".nix-profile"))
  pure $ case r of
    Left (_ :: IOException) -> Nothing
    Right l                 -> Just (home </> l)   -- absolute targets win over $HOME

-- | (generation number, date, link path), oldest first.
profileLinks :: FilePath -> IO [(Int, Text, FilePath)]
profileLinks base = do
  let dir = takeDirectory base
      pre = T.pack (takeFileName base) <> "-"
  names <- listDirectory dir `catch` \(_ :: IOException) -> pure []
  gens <- fmap catMaybes . forM names $ \nm ->
    case T.stripPrefix pre (T.pack nm) >>= T.stripSuffix "-link" >>= readMaybe . T.unpack of
      Nothing -> pure Nothing
      Just n -> do
        let l = dir </> nm
        d <- (getModificationTime l >>= fmtTime) `catch` \(_ :: IOException) -> pure ""
        pure (Just (n, d, l))
  pure (sortOn (\(n, _, _) -> n) gens)

-- | Choose a profile and two of its generations → (older link, newer link).
pickGenerations :: IO (Maybe (FilePath, FilePath))
pickGenerations = do
  home <- getHomeDirectory
  sysE <- doesPathExist "/nix/var/nix/profiles/system"
  userBase <- userProfileBase
  let hm = home </> ".local/state/nix/profiles/home-manager"
  hmE <- doesPathExist hm
  let labels = [ ("System", "/nix/var/nix/profiles/system") | sysE ]
            ++ [ ("My profile", b) | Just b <- [userBase] ]
            ++ [ ("Home Manager", hm) | hmE ]
  if null labels then warn "no profiles with generations found" >> pure Nothing else do
    mc <- Gum.choose "Which profile?" (map fst labels)
    case mc >>= (`lookup` labels) of
      Nothing -> pure Nothing
      Just base -> do
        gens <- profileLinks base
        if length gens < 2 then warn "need at least two generations" >> pure Nothing else do
          let lbl (n, d, l) = "#" <> padR 5 (T.pack (show n)) <> " " <> padR 17 d <> " " <> T.pack l
              labs = [ (lbl g, g) | g <- gens ]
          msel <- Gum.chooseMany (Just 2) "select two generations" (map fst labs)
          case msel of
            Nothing -> pure Nothing
            Just sel -> case sortOn (\(n, _, _) -> n) [ g | s <- sel, Just g <- [lookup s labs] ] of
              [(_, _, a), (_, _, b)] -> pure (Just (a, b))
              _ -> warn "select exactly two" >> pure Nothing

diffClosures :: String -> String -> IO ()
diffClosures a b = runPaged ["nix", "store", "diff-closures", a, b]

menuDiff :: IO ()
menuDiff = do
  mc <- Gum.choose "Diff closures" ["Between two generations of a profile", "Between any two targets"]
  case mc of
    Nothing -> pure ()
    Just c
      | "Between two gen" `T.isPrefixOf` c ->
          pickGenerations >>= maybe pause (uncurry diffClosures)
      | otherwise -> do
          ma <- askTarget "before / older"
          mb <- maybe (pure Nothing) (const (askTarget "after / newer")) ma
          forM_ ((,) <$> ma <*> mb) $ \(a, b) -> diffClosures (T.unpack a) (T.unpack b)

-- inspect & debug ------------------------------------------------------------

askTarget :: Text -> IO (Maybe Text)
askTarget hdr = do
  home <- getHomeDirectory
  sysE <- doesPathExist "/run/current-system"
  profE <- doesPathExist (home </> ".nix-profile")
  let items = [ "Current system (/run/current-system)" | sysE ]
           ++ [ "My profile (~/.nix-profile)" | profE ]
           ++ [ "A nixpkgs package…", "Any installable or store path…" ]
  mc <- Gum.choose hdr items
  case mc of
    Nothing -> pure Nothing
    Just c
      | "Current" `T.isPrefixOf` c    -> pure (Just "/run/current-system")
      | "My profile" `T.isPrefixOf` c -> pure (Just (T.pack (home </> ".nix-profile")))
      | "A nixpkgs" `T.isPrefixOf` c  -> do
          ch <- getChannel
          fmap ((ch <> "#") <>) <$> askNE "package" "attribute, e.g. glibc"
      | otherwise -> askNE "installable / path" "e.g. .#foo, /nix/store/…, ./result"

menuClosure :: IO ()
menuClosure = do
  mt <- askTarget "Closure of…"
  forM_ mt $ \t -> do
    dim "reading closure (this may build or download the target)…"
    closureReport t >>= either bad Gum.pager

-- | "Why does A depend on B?" — B comes from a (possibly huge) closure listing, so: fzf.
menuWhy :: IO ()
menuWhy = do
  ma <- askTarget "Which package or system do you want to inspect?"
  forM_ ma $ \a -> do
    dim ("reading the closure of " <> a <> "…")
    items <- closureCandidates a
    mb <- Gum.pickOrType ("why is this in " <> a <> "? (type to filter)") items
    forM_ mb $ \b0 -> do
      b <- normalizeTarget b0
      mm <- Gum.choose "" ["shortest path", "all paths"]
      forM_ mm $ \m -> do
        void (runCmd (["nix", "why-depends"] ++ [ "--all" | "all" `T.isPrefixOf` m ] ++ [T.unpack a, T.unpack b]))
        pause

-- | Pick something from the store (or type an installable).
pickStoreTarget :: StoreMode -> Text -> IO (Maybe Text)
pickStoreTarget mode hdr = do
  items <- storeCandidates mode
  mr <- Gum.pickOrType hdr items
  maybe (pure Nothing) (fmap Just . normalizeTarget) mr

menuBuildLog :: IO ()
menuBuildLog = do
  mr <- pickStoreTarget SOut "build log of… (or type an installable, e.g. nixpkgs#hello)"
  forM_ mr $ \r -> pagedCmd ["nix", "log", T.unpack r]

menuDerivation :: IO ()
menuDerivation = do
  mr <- pickStoreTarget SAll "show derivation of… (or type an installable, e.g. .#foo)"
  forM_ mr $ \r -> do
    out <- capture ["nix", "derivation", "show", T.unpack r]
    out' <- maybe (captureAll ["nix", "show-derivation", T.unpack r]) pure out
    case parse out' of
      Nothing -> Gum.pager out'
      Just val -> Gum.pager (pretty val)

menuBuildDebug :: IO ()
menuBuildDebug = do
  mr <- pickStoreTarget SOut "rebuild/debug… (or type .#foo / nixpkgs#hello)"
  forM_ mr $ \r0 -> do
    r <- toBuildTarget r0
    let table = [ ("-L  stream build logs", "-L")
                , ("--keep-failed  keep the build directory when it fails", "--keep-failed")
                , ("--check  rebuild & compare against the existing result (reproducibility)", "--check")
                , ("--show-trace", "--show-trace")
                , ("--impure", "--impure") ]
    msel <- Gum.chooseMany Nothing "build options (space to toggle)" (map fst table)
    let flags = [ f | s <- fromMaybe [] msel, Just f <- [lookup s table] ]
    void (runCmd (["nix", "build", T.unpack r, "--no-link"] ++ flags))
    pause

menuRepl :: IO ()
menuRepl = do
  mc <- Gum.choose "nix repl" ["Plain repl", "With nixpkgs in scope", "Load a flake directory"]
  case mc of
    Just "Plain repl" -> void (runCmd ["nix", "repl"])
    Just "With nixpkgs in scope" -> do
      ch <- getChannel
      void (runCmd ["nix", "repl", "--expr", "import (builtins.getFlake \"" <> T.unpack ch <> "\") {}"])
    Just "Load a flake directory" -> do
      cwd <- getCurrentDirectory
      md <- askDef "flake dir" "" (T.pack cwd)
      forM_ md $ \d -> do
        home <- getHomeDirectory
        let d' = case T.unpack d of ('~' : r) -> home <> r; x -> x
        ex <- doesDirectoryExist d'
        if ex then canonicalizePath d' >>= \p -> void (runCmd ["nix", "repl", "--expr", "builtins.getFlake \"" <> p <> "\""])
              else warn "no such directory" >> pause
    _ -> pure ()

menuStoreHealth :: IO ()
menuStoreHealth = do
  mc <- Gum.choose "Store health" ["Verify store contents", "Repair store (sudo)", "nix doctor", "Show nix config"]
  case mc of
    Just "Verify store contents" -> runCmd ["nix", "store", "verify", "--all", "--no-trust"] >> pause
    Just "Repair store (sudo)" -> do
      c <- confirm "Run a full check-contents repair? This can take a long time."
      when c $ do
        s <- sudoPrefix
        void (runCmd (s ++ ["nix-store", "--verify", "--check-contents", "--repair"]))
        pause
    Just "nix doctor" -> runCmd ["nix", "doctor"] >> pause
    Just "Show nix config" -> pagedCmd ["nix", "config", "show"]
    _ -> pure ()

menuVulnix :: IO ()
menuVulnix = do
  hv <- have "vulnix"
  unless hv $ do
    ch <- getChannel
    warn ("vulnix isn't installed — using it via: nix shell " <> ch <> "#vulnix")
  mc <- Gum.choose "Scan for known vulnerabilities (CVEs)" ["Running system", "My profile", "All GC roots", "A package or path…"]
  forM_ mc $ \c -> do
    warn "the first run downloads the NVD database and can take a while"
    let vx args = toolCmd "vulnix" "vulnix" args >>= void . runCmd
    case c of
      "Running system" -> vx ["-S"]
      "My profile" -> do
        home <- getHomeDirectory
        p <- canonicalizePath (home </> ".nix-profile")
        vx [p]
      "All GC roots" -> vx ["-G"]
      _ -> do
        mr <- askNE "installable / path" "e.g. nixpkgs#openssl"
        forM_ mr $ \r -> resolveTarget r >>= vx
    pause

menuInspect :: IO ()
menuInspect = menuLoop "Inspect & debug"
  [ opt "Closure sizes" menuClosure
  , opt "Why does X depend on Y?" menuWhy
  , opt "Diff closures / generations" menuDiff
  , opt "Build log" menuBuildLog
  , opt "Show derivation" menuDerivation
  , opt "Debug a build…" menuBuildDebug
  , opt "nix repl" menuRepl
  , opt "Store health" menuStoreHealth
  , opt "Vulnerability scan (vulnix)" menuVulnix
  ]
