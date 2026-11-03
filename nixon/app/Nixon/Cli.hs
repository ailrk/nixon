{-# LANGUAGE OverloadedStrings #-}
module Nixon.Cli (cli, usage) where

import Control.Monad
import Data.List (partition)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Gum (bad)
import Nixon.Core
import Nixon.Json
import Nixon.Nix
import System.Directory (doesFileExist)
import System.Exit

usage :: IO ()
usage = do
  sd <- stateDir
  TIO.putStr $ T.unlines
    [ "nixon " <> version <> " — a TUI/CLI front-end for Nix"
    , ""
    , "Interactive:"
    , "  nixon                          full menu"
    , "  nixon packages|profile|flake|inspect|options|maintenance|caches|channels|nixos|home|darwin|favorites"
    , ""
    , "Scriptable:"
    , "  nixon search [--json] QUERY…         search the active channel (TSV: attr, version, description)"
    , "  nixon shell PKG…                     nix shell with packages from the active channel"
    , "  nixon run PKG [-- ARGS…]             nix run a package"
    , "  nixon install PKG…                   nix profile install"
    , "  nixon closure TARGET                 closure size report"
    , "  nixon why A B                        nix why-depends A B"
    , "  nixon diff A B                       nix store diff-closures A B"
    , "  nixon options nixos|hm TEXT          search option names (TSV: name, type; case-insensitive substring)"
    , "  nixon locate FILE [--regex]          which package provides FILE (needs nix-index)"
    , "  nixon cached INSTALLABLE             is it in the binary caches?"
    , "  nixon old PKG [VERSION]              versions of PKG on nixhub, or the pinned flake ref for VERSION"
    , "  nixon outputs [DIR]                  flake outputs for this system (TSV: attr, type)"
    , "  nixon roots                          GC roots with closure sizes"
    , "  nixon gc [--all | --older-than AGE] [--dry-run]"
    , "  nixon channel [get | set REF]        show or set the nixpkgs ref nixon uses"
    , "  nixon favs [add|rm PKG…]             list or edit favorites"
    , "  nixon history                        show command history"
    , ""
    , "Global flags:  -y/--yes   --channel REF"
    , ""
    , "State lives in: " <> T.pack sd
    ]

finish :: Bool -> IO ()
finish ok' = unless ok' exitFailure

cli :: [String] -> IO ()
cli [] = dieWith "unknown command '' (see nixon --help)"
cli (cmd : args) = case cmd of
  "search" -> do
    let (js, qs) = partition (== "--json") args
    when (null qs) (dieWith "usage: nixon search [--json] QUERY…")
    if not (null js)
      then do
        ch <- getChannel
        capture (["nix", "search", T.unpack ch] ++ qs ++ ["--json"]) >>= TIO.putStr . fromMaybe ""
      else searchPkgs (map T.pack qs) >>= mapM_ (\p -> TIO.putStrLn (T.intercalate "\t" [pkAttr p, pkVersion p, pkDesc p]))
  "shell" -> do
    when (null args) (dieWith "usage: nixon shell PKG…")
    refs <- mapM (toRef . T.pack) args
    runCmd ("nix" : "shell" : map T.unpack refs) >>= finish
  "run" -> case args of
    [] -> dieWith "usage: nixon run PKG [-- ARGS…]"
    (p : rest) -> do
      ref <- toRef (T.pack p)
      runCmd (["nix", "run", T.unpack ref, "--"] ++ dropWhile (== "--") (take 1 rest) ++ drop 1 rest) >>= finish
  "install" -> do
    when (null args) (dieWith "usage: nixon install PKG…")
    refs <- mapM (toRef . T.pack) args
    runCmd (["nix", "profile", "install"] ++ map T.unpack refs) >>= finish
  "closure" -> case args of
    [t] -> closureReport (T.pack t) >>= either (\e -> bad e >> exitFailure) TIO.putStr
    _   -> dieWith "usage: nixon closure TARGET"
  "why"  -> case args of [a, b] -> runCmd ["nix", "why-depends", a, b] >>= finish; _ -> dieWith "usage: nixon why A B"
  "diff" -> case args of [a, b] -> runCmd ["nix", "store", "diff-closures", a, b] >>= finish; _ -> dieWith "usage: nixon diff A B"
  "options" -> case args of
    (k : q : _) -> do
      kind <- case k of "nixos" -> pure OptNixos; "hm" -> pure OptHm; _ -> dieWith "usage: nixon options nixos|hm TEXT"
      r <- optionsJson kind
      case r of
        Left e -> bad e >> exitFailure
        Right f -> do
          mv <- parse <$> TIO.readFile f
          forM_ (maybe [] members mv) $ \(n, o) ->
            when (T.toLower (T.pack q) `T.isInfixOf` T.toLower n) $
              TIO.putStrLn (n <> "\t" <> fromMaybe "" (key "type" o >>= strOf))
    _ -> dieWith "usage: nixon options nixos|hm TEXT"
  "locate" -> case args of
    (f : rest) -> do
      db <- locateDb
      ex <- doesFileExist db
      unless ex (dieWith "no nix-index database; run: nixon packages (Update the file index)")
      rs <- locateLines (if "--regex" `elem` rest then Regex else Exact) (T.pack f)
      forM_ rs $ \(a, p) -> TIO.putStrLn (a <> "\t" <> p)
    _ -> dieWith "usage: nixon locate FILE [--regex]"
  "cached" -> case args of
    [i] -> toRef (T.pack i) >>= cachedReport
    _   -> dieWith "usage: nixon cached INSTALLABLE"
  "old" -> case args of
    [p] -> nixhubVersions (T.pack p) >>= mapM_ TIO.putStrLn
    [p, v] -> nixhubResolve (T.pack p) (T.pack v) >>= maybe exitFailure TIO.putStrLn
    _ -> dieWith "usage: nixon old PKG [VERSION]"
  "outputs" -> flakeOutputs (fromMaybe "." (listToMaybe args)) >>= mapM_ (\o -> TIO.putStrLn (outAttr o <> "\t" <> outType o))
  "roots" -> rootsLines >>= mapM_ TIO.putStrLn . roots2text
  "gc" -> gc args
  "channel" -> case args of
    [] -> getChannel >>= TIO.putStrLn
    ["get"] -> getChannel >>= TIO.putStrLn
    ["set", r] -> setChannel (T.pack r)
    _ -> dieWith "usage: nixon channel [get | set REF]"
  "history" -> historyLines >>= mapM_ TIO.putStrLn
  _ -> dieWith ("unknown command '" <> T.pack cmd <> "' (see nixon --help)")

gc :: [String] -> IO ()
gc = go ("plain" :: String) False Nothing
  where
    go mode dry age as = case as of
      ("--all" : r)        -> go "all" dry age r
      ("--older-than" : a : r) -> go "age" dry (Just a) r
      ("--dry-run" : r)    -> go mode True age r
      []                   -> run' mode dry age
      (x : _)              -> dieWith ("unknown gc option '" <> T.pack x <> "'")
    run' mode dry age
      | dry = void (spawn ["nix-collect-garbage", "--dry-run"])
      | otherwise = do
          c <- confirm ("Run garbage collection (" <> T.pack mode <> ")?")
          unless c exitFailure
          case (mode, age) of
            ("all", _)        -> runCmd ["nix-collect-garbage", "-d"] >>= finish
            ("age", Just a)   -> runCmd ["nix-collect-garbage", "--delete-older-than", a] >>= finish
            ("age", Nothing)  -> dieWith "--older-than needs an age like 14d"
            _                 -> runCmd ["nix-collect-garbage"] >>= finish

_t :: Text
_t = ""
