{-# LANGUAGE OverloadedStrings #-}
-- | Everything that talks to nix and returns data, no menus here.
module Nixon.Nix where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad
import Data.Char (isAlphaNum, isAsciiLower, isDigit)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Gum (Item (..), bad, dim, ok, warn)
import Nixon.Core
import Nixon.Json
import Numeric (showFFloat)
import System.Directory
import System.Exit
import System.FilePath
import System.Info (arch, os)
import Text.Read (readMaybe)

curSystem :: Text
curSystem = T.pack (arch <> "-" <> os)

-- package search ------------------------------------------------------------

data Pkg = Pkg { pkAttr, pkVersion, pkDesc, pkPname :: Text }

searchPkgs :: [Text] -> IO [Pkg]
searchPkgs qs = do
  ch <- getChannel
  m <- capture (["nix", "search", T.unpack ch] ++ map T.unpack qs ++ ["--json"])
  pure $ case m >>= parse of
    Nothing -> []
    Just v  -> sortOn pkAttr
      [ Pkg (T.intercalate "." (drop 2 (T.splitOn "." k)))
            (s "version" o) (T.map (\c -> if c == '\n' || c == '\t' then ' ' else c) (s "description" o)) (s "pname" o)
      | (k, o) <- members v ]
  where s f o = fromMaybe "" (key f o >>= strOf)

packageInfo :: Text -> IO Text
packageInfo p = do
  ref <- toRef p
  m <- capture ["nix", "eval", "--json", T.unpack ref <> ".meta"]
  pure $ case m >>= parse of
    Nothing -> p <> "\n  could not evaluate meta\n"
    Just v  -> T.unlines
      [ p
      , "  description  " <> f "description" v
      , "  homepage     " <> f "homepage" v
      , "  main program " <> f "mainProgram" v
      , "  license      " <> lic v ]
  where
    f k v = fromMaybe "-" (key k v >>= strOf)
    spdx x = (key "spdxId" x >>= strOf) <|> (key "shortName" x >>= strOf)
    lic v = case key "license" v of
      Just a@(Array _) -> T.intercalate ", " [ fromMaybe "?" (spdx x) | x <- elems a ]
      Just o@(Object _) -> fromMaybe "-" (spdx o)
      _ -> "-"

-- store paths & closures ------------------------------------------------------------

human :: Integer -> Text
human n = go (fromIntegral n :: Double) (0 :: Int)
  where
    us = ["B", "K", "M", "G", "T"] :: [Text]
    go x i
      | x >= 1024 && i < 4 = go (x / 1024) (i + 1)
      | i == 0             = T.pack (show (round x :: Integer)) <> "B"
      | otherwise          = T.pack (showFFloat (Just 1) x "") <> (us !! i)

-- | Store path(s) for an installable, or an existing path/symlink.
resolveTarget :: Text -> IO [FilePath]
resolveTarget t = do
  isLink <- pathIsSymbolicLink p `catch` \(_ :: IOException) -> pure False
  if isLink || "/nix/store/" `T.isPrefixOf` t
    then (: []) <$> canonicalizePath p
    else maybe [] (map T.unpack . filter (not . T.null) . T.lines)
           <$> capture ["nix", "build", "--no-link", "--print-out-paths", p]
  where p = T.unpack t

-- | (narSize, path), biggest first. Handles both old (array) and new (object) path-info JSON.
closureEntries :: Value -> [(Integer, Text)]
closureEntries v = sortOn (Down . fst) $ case v of
  Array _  -> [ (sz o, fromMaybe "" (key "path" o >>= strOf)) | o <- elems v ]
  Object _ -> [ (sz o, p) | (p, o) <- members v ]
  _        -> []
  where sz o = fromMaybe 0 (key "narSize" o >>= intOf)

closureReport :: Text -> IO (Either Text Text)
closureReport target = do
  paths <- resolveTarget target
  if null paths then pure (Left ("could not resolve '" <> target <> "'")) else do
    m <- capture (["nix", "path-info", "-r", "--json"] ++ paths)
    pure $ case closureEntries <$> (m >>= parse) of
      Just es@(_ : _) ->
        let total = sum (map fst es)
        in Right $ T.unlines $
             [ "Closure of " <> target <> " — " <> T.pack (show (length es)) <> " paths, " <> human total <> " total", "" ]
             ++ [ padL 8 (human sz) <> "  " <> p | (sz, p) <- es ]
      _ -> Left ("could not read the closure of '" <> target <> "'")

-- | "name   /nix/store/hash-name" items for full store paths, sorted by name.
storeItems :: [Text] -> [Item]
storeItems ps =
  [ Item p (padR 52 n <> " " <> p) Nothing
  | (n, p) <- sortOn (T.toLower . fst) [ (T.drop 33 base, p) | p <- ps, Just base <- [valid p] ] ]
  where
    valid p = do
      b <- T.stripPrefix "/nix/store/" p
      guard (T.length b > 33 && T.index b 32 == '-' && T.all (\c -> isAsciiLower c || isDigit c) (T.take 32 b))
      pure b

data StoreMode = SAll | SOut | SDrv

storeCandidates :: StoreMode -> IO [Item]
storeCandidates mode = do
  names <- listDirectory "/nix/store" `catch` \(_ :: IOException) -> pure []
  let ok' n = not (any (`T.isSuffixOf` n) [".lock", ".chroot", ".check"])
      isDrv = (".drv" `T.isSuffixOf`)
      sel n = case mode of SAll -> True; SOut -> not (isDrv n); SDrv -> isDrv n
  pure (storeItems [ "/nix/store/" <> n | nm <- names, let n = T.pack nm, ok' n, sel n ])

closureCandidates :: Text -> IO [Item]
closureCandidates t = do
  ps <- resolveTarget t
  if null ps then bad ("could not resolve '" <> t <> "'") >> pure [] else
    maybe [] (storeItems . T.lines) <$> capture (["nix", "path-info", "-r"] ++ ps)

-- | Turn an output path into its derivation (drv^*) for rebuilds.
toBuildTarget :: Text -> IO Text
toBuildTarget p
  | ".drv" `T.isSuffixOf` p = pure (p <> "^*")
  | "/nix/store/" `T.isPrefixOf` p = do
      d <- fmap T.strip <$> capture ["nix", "path-info", "--derivation", T.unpack p]
      pure $ case d of
        Just x | not (T.null x), x /= "unknown-deriver" -> x <> "^*"
        _ -> p
  | otherwise = pure p

-- find by file (nix-locate) ------------------------------------------------------------

locateDb :: IO FilePath
locateDb = getXdgDirectory XdgCache "nix-index/files"

locateUpdate :: IO Bool
locateUpdate = do
  hc <- have "curl"
  if not hc then bad "curl is required" >> pure False else do
    db <- locateDb
    createDirectoryIfMissing True (takeDirectory db)
    let file = "index-" <> arch <> "-" <> os
        tmp = db <> ".part"
    dim "downloading prebuilt index from nix-community/nix-index-database…"
    rc <- spawn ["curl", "-fL", "--progress-bar", "-o", tmp,
                 "https://github.com/nix-community/nix-index-database/releases/latest/download/" <> file]
    case rc of
      ExitSuccess -> renameFile tmp db >> ok "index updated" >> pure True
      _           -> bad "download failed" >> pure False

data Match = Exact | Regex

-- | (attr, path) pairs.
locateLines :: Match -> Text -> IO [(Text, Text)]
locateLines m pat = do
  cmd <- toolCmd "nix-locate" "nix-index" (["--top-level"] ++ [ "-w" | Exact <- [m] ] ++ [T.unpack pat])
  out <- fromMaybe "" <$> capture cmd
  pure [ (a, last rest) | l <- T.lines out, (a : rest@(_ : _)) <- [T.words l] ]

stripOutput :: Text -> Text
stripOutput t = case T.breakOnEnd "." t of
  (pre, suf) | suf `elem` outs, not (T.null pre) -> T.dropEnd 1 pre
  _ -> t
  where outs = ["out", "bin", "dev", "lib", "doc", "man", "info", "static", "devdoc", "debug", "data"]

-- older versions (nixhub) ------------------------------------------------------------

nixhubApi :: String
nixhubApi = "https://search.devbox.sh/v2"

curlJson :: String -> [(String, Text)] -> IO (Maybe Value)
curlJson ep params = do
  m <- capture (["curl", "-fsSG", "-m", "20"]
                ++ concat [ ["--data-urlencode", k <> "=" <> T.unpack v] | (k, v) <- params ]
                ++ [nixhubApi <> ep])
  pure (m >>= parse)

nixhubVersions :: Text -> IO [Text]
nixhubVersions name = do
  mv <- curlJson "/pkg" [("name", name)]
  pure [ v | Just j <- [mv], r <- maybe [] elems (key "releases" j), Just v <- [key "version" r >>= strOf] ]

-- | Pinned flake installable (github:owner/repo/rev#attr) for a version on this system.
nixhubResolve :: Text -> Text -> IO (Maybe Text)
nixhubResolve name ver = do
  mv <- curlJson "/resolve" [("name", name), ("version", ver)]
  pure $ do
    j  <- mv
    fi <- at ["systems", curSystem, "flake_installable"] j
    owner <- at ["ref", "owner"] fi >>= strOf
    repo  <- at ["ref", "repo"] fi >>= strOf
    rev   <- at ["ref", "rev"] fi >>= strOf
    attr  <- key "attr_path" fi >>= strOf
    pure ("github:" <> owner <> "/" <> repo <> "/" <> rev <> "#" <> attr)

-- flake outputs ------------------------------------------------------------

data Out = Out { outAttr :: Text, outType :: Text, outVal :: Value }

-- | Outputs of a flake that make sense on this system.
flakeOutputs :: FilePath -> IO [Out]
flakeOutputs dir = do
  m <- capture ["nix", "flake", "show", "--json", dir]
  pure $ case m >>= parse of
    Nothing -> []
    Just v  -> filter relevant (walk [] v)
  where
    walk p v@(Object _)
      | Just (String t) <- key "type" v = [Out (T.intercalate "." (reverse p)) t v]
      | otherwise = concat [ walk (k : p) c | (k, c) <- members v ]
    walk _ _ = []
    relevant o = case T.splitOn "." (outAttr o) of
      (_ : s : _) | any (`T.isSuffixOf` s) ["-linux", "-darwin"], s /= curSystem -> False
      _ -> True

outPreview :: Out -> Text
outPreview o = T.intercalate "\n" [ k <> ": " <> render v | (k, v) <- members (outVal o) ]
  where render (String s) = s
        render v = pretty v

flakeInputs :: FilePath -> IO [Text]
flakeInputs dir = do
  m <- withCurrentDirectory dir (capture ["nix", "flake", "metadata", "--json"])
  pure (maybe [] (map fst . members) (m >>= parse >>= at ["locks", "nodes", "root", "inputs"]))

-- option search (NixOS / Home Manager) ------------------------------------------------------------

data OptKind = OptNixos | OptHm

nixosExpr :: Bool -> String
nixosExpr withDoc = unlines
  [ "{ ref }:"
  , "let"
  , "  flake  = builtins.getFlake ref;"
  , "  system = builtins.currentSystem;"
  , if withDoc then "  pkgs   = import flake { inherit system; };" else ""
  , "  eval   = flake.lib.nixosSystem {"
  , "    inherit system;"
  , "    modules = [ { fileSystems.\"/\".device = \"none\"; boot.loader.grub.enable = false; system.stateVersion = \"24.11\"; } ];"
  , "  };"
  , "in " ++ if withDoc
               then "(pkgs.nixosOptionsDoc { options = eval.options; warningsAreErrors = false; }).optionsJSON"
               else "eval.config.system.build.manual.optionsJSON"
  ]

findNamed :: FilePath -> String -> IO (Maybe FilePath)
findNamed root name = do
  isDir <- doesDirectoryExist root
  if not isDir
    then pure (if takeFileName root == name then Just root else Nothing)
    else do
      es <- listDirectory root `catch` \(_ :: IOException) -> pure []
      firstJustM (\e -> findNamed (root </> e) name) es
  where
    firstJustM _ [] = pure Nothing
    firstJustM f (x : xs) = f x >>= maybe (firstJustM f xs) (pure . Just)

-- | Path of a cached options.json for the current revision (built on first use).
optionsJson :: OptKind -> IO (Either Text FilePath)
optionsJson kind = do
  ch <- getChannel
  let (flake, tag) = case kind of
        OptNixos -> (ch, "nixos" :: String)
        OptHm    -> ("github:nix-community/home-manager", "hm")
      fl = T.unpack flake
  mrev <- capture ["nix", "flake", "metadata", "--json", fl]
  let rev = do t <- mrev
               j <- parse t
               (at ["locked", "rev"] j <|> at ["locked", "narHash"] j) >>= strOf
  case rev of
    Nothing -> pure (Left ("could not resolve " <> flake))
    Just r -> do
      cd <- cacheDir
      let cache = cd </> ("options-" ++ tag ++ "-" ++ map (\c -> if isAlphaNum c then c else '_') (T.unpack r) ++ ".json")
      cached <- doesFileExist cache
      if cached then pure (Right cache) else do
        dim ("building the " <> T.pack tag <> " option index for " <> flake <> " (first run only; cached afterwards)…")
        let build xs = captureOut (["nix", "build", "--no-link", "--print-out-paths"] ++ xs)
            nixos e = build ["--impure", "--argstr", "ref", fl, "--expr", e]
        mout <- case kind of
          OptNixos -> do a <- nixos (nixosExpr True)
                         maybe (nixos (nixosExpr False)) (pure . Just) a
          OptHm    -> build [fl <> "#docs-json"]
        case fmap (T.unpack . T.strip . firstOr "" . T.lines) mout of
          Nothing -> pure (Left "could not build the option index")
          Just out -> do
            f <- findNamed out "options.json"
            case f of
              Nothing -> pure (Left ("options.json not found in " <> T.pack out))
              Just src -> do
                copyFile src cache
                p <- getPermissions cache
                setPermissions cache (setOwnerWritable True p)
                pure (Right cache)

optionDetail :: Text -> Value -> Text
optionDetail k o = T.unlines
  [ k
  , "  type:     " <> fromMaybe "-" (key "type" o >>= strOf)
  , "  default:  " <> txtOf (key "default" o)
  , "  example:  " <> txtOf (key "example" o)
  , ""
  , txtOf (key "description" o)
  , ""
  , "declared in:"
  , T.unlines [ "  " <> d | d <- maybe [] (map decl . elems) (key "declarations" o) ]
  ]
  where
    txtOf Nothing = "-"
    txtOf (Just Null) = "-"
    txtOf (Just (String s)) = s
    txtOf (Just v@(Object _)) = fromMaybe (pretty v) (key "text" v >>= strOf)
    txtOf (Just v) = pretty v
    decl (String s) = s
    decl v = fromMaybe (pretty v) ((key "name" v >>= strOf) <|> (key "url" v >>= strOf))

-- GC roots ------------------------------------------------------------

-- | (closure bytes, root, target), biggest first.
rootsLines :: IO [(Integer, Text, Text)]
rootsLines = do
  out <- fromMaybe "" <$> capture ["nix-store", "--gc", "--print-roots"]
  let roots = [ (a, T.drop 4 rest)
              | l <- T.lines out, let (a, rest) = T.breakOn " -> " l, not (T.null rest)
              , not ("{" `T.isPrefixOf` a), not ("/proc/" `T.isPrefixOf` a) ]
      targets = S.toList (S.fromList (map snd roots))
  sizes <- if null roots then pure M.empty else do
    m <- capture (["nix", "path-info", "-S"] ++ map T.unpack targets)
    pure (M.fromList [ (p, n) | l <- T.lines (fromMaybe "" m), [p, s] <- [T.words l], Just n <- [readMaybe (T.unpack s)] ])
  pure (sortOn (\(n, _, _) -> Down n) [ (M.findWithDefault 0 t sizes, r, t) | (r, t) <- roots ])

roots2text :: [(Integer, Text, Text)] -> [Text]
roots2text rs = [ padL 8 (human n) <> "  " <> r <> "  ->  " <> t | (n, r, t) <- rs ]

-- caches ------------------------------------------------------------

nixConf :: Text -> IO Text
nixConf k = do
  m <- capture ["nix", "config", "show", T.unpack k]
  case m of
    Just o  -> pure (T.strip o)
    Nothing -> do
      o <- fromMaybe "" <$> capture ["nix", "show-config"]
      pure (T.strip (T.unwords [ T.strip (T.drop (T.length k + 3) l) | l <- T.lines o, (k <> " = ") `T.isPrefixOf` l ]))

httpCode :: Int -> String -> IO Text
httpCode t url = maybe "000" T.strip <$> capture ["curl", "-s", "-o", "/dev/null", "-m", show t, "-w", "%{http_code}", url]

substituterReport :: IO Text
substituterReport = do
  subs <- T.words <$> nixConf "substituters"
  ls <- forM subs $ \s ->
    if "http" `T.isPrefixOf` s
      then do
        let url = T.unpack s <> "/nix-cache-info"
        code <- httpCode 6 url
        body <- fromMaybe "" <$> capture ["curl", "-s", "-m", "6", url]
        let prio = listToMaybe [ T.strip p | l <- T.lines body, Just p <- [T.stripPrefix "Priority:" l] ]
        pure $ if code == "200"
          then "✓ " <> padR 44 s <> " priority " <> fromMaybe "-" prio
          else "✗ " <> padR 44 s <> " HTTP " <> code
      else pure ("– " <> padR 44 s <> " (not HTTP; skipped)")
  ks <- T.words <$> nixConf "trusted-public-keys"
  pure (T.unlines (ls ++ ["", "trusted public keys:"] ++ map ("  " <>) ks))

cachedReport :: Text -> IO ()
cachedReport ref = do
  mo <- capture ["nix", "eval", "--raw", T.unpack ref <> ".outPath"]
  case T.strip <$> mo of
    Nothing -> bad ("could not evaluate '" <> ref <> "'")
    Just out -> do
      let h = T.takeWhile (/= '-') (fromMaybe out (T.stripPrefix "/nix/store/" out))
      TIO.putStrLn out
      loc <- capture ["nix", "path-info", T.unpack out]
      when (isJust loc) (ok "already in your local store")
      subs <- T.words <$> nixConf "substituters"
      forM_ (filter ("http" `T.isPrefixOf`) subs) $ \s -> do
        code <- httpCode 8 (T.unpack s <> "/" <> T.unpack h <> ".narinfo")
        if code == "200"
          then ok ("cached on " <> s)
          else warn ("not on " <> s <> " (HTTP " <> code <> ") — it would be built locally")
