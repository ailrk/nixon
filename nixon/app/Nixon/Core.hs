{-# LANGUAGE OverloadedStrings #-}
-- | State/config files, subprocess helpers and the menu-loop combinator.
module Nixon.Core where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import qualified Gum
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.User (getEffectiveUserID)
import System.Process
import Data.Version (showVersion)
import qualified Paths_nixon

-- Remember the cursor position at each level of the menu
type CursorPos = [Int]

version :: Text
version = T.pack (showVersion Paths_nixon.version)

--  global flags ------------------------------------------------------------
-- Two process-wide settings (active channel, --yes)
-- IORefs keep every menu a plain IO action.

{-# NOINLINE channelRef #-}
channelRef :: IORef Text
channelRef = unsafePerformIO (newIORef "nixpkgs")

{-# NOINLINE assumeYesRef #-}
assumeYesRef :: IORef Bool
assumeYesRef = unsafePerformIO (newIORef False)

getChannel :: IO Text
getChannel = readIORef channelRef

--  state dir ------------------------------------------------------------

stateDir, cacheDir :: IO FilePath
stateDir = getXdgDirectory XdgState "nixon"
cacheDir = getXdgDirectory XdgCache "nixon"

stateFile :: FilePath -> IO FilePath
stateFile f = (</> f) <$> stateDir

initState :: IO ()
initState = do
  sd <- stateDir
  cd <- cacheDir
  createDirectoryIfMissing True sd
  createDirectoryIfMissing True cd
  forM_ ["history"] $ \f -> do
    p <- stateFile f
    ex <- doesFileExist p
    unless ex (TIO.writeFile p "")
  h <- stateFile "history"
  ls <- T.lines <$> TIO.readFile h
  when (length ls > 600) $ TIO.writeFile h (T.unlines (drop (length ls - 500) ls))
  envRef <- lookupEnv "NIXON_NIXPKGS"
  cfg <- stateFile "config"
  hasCfg <- doesFileExist cfg
  fromCfg <- if hasCfg
    then listToMaybe . reverse . mapMaybe (T.stripPrefix "NIXPKGS_REF=") . T.lines <$> TIO.readFile cfg
    else pure Nothing
  writeIORef channelRef $ case envRef of
    Just e | not (null e) -> T.pack e
    _                     -> fromMaybe "nixpkgs" fromCfg

setChannel :: Text -> IO ()
setChannel c = do
  writeIORef channelRef c
  p <- stateFile "config"
  TIO.writeFile p ("NIXPKGS_REF=" <> c <> "\n")
  Gum.ok ("nixon now uses '" <> c <> "' for search, shells and installs")

-- ------------------------------------------------------------ history / favourites

record :: Text -> IO ()
record cmd = do
  t <- (round <$> getPOSIXTime) :: IO Int
  p <- stateFile "history"
  TIO.appendFile p (T.pack (show t) <> "\t" <> cmd <> "\n")

fmtTime :: UTCTime -> IO Text
fmtTime t = do
  z <- utcToLocalZonedTime t
  pure (T.pack (formatTime defaultTimeLocale "%F %R" z))

-- | (date, command), newest first.
historyEntries :: IO [(Text, Text)]
historyEntries = do
  p <- stateFile "history"
  ls <- T.lines <$> TIO.readFile p
  forM (reverse ls) $ \l -> do
    let (ts, rest) = T.breakOn "\t" l
        cmd = T.drop 1 rest
    d <- case reads (T.unpack ts) :: [(Integer, String)] of
      [(n, "")] -> fmtTime (posixSecondsToUTCTime' n)
      _         -> pure ts
    pure (d, cmd)
  where posixSecondsToUTCTime' = posixSecondsToUTCTime . fromIntegral

historyLines :: IO [Text]
historyLines = map (\(d, c) -> d <> "  " <> c) <$> historyEntries

clearHistory :: IO ()
clearHistory = stateFile "history" >>= \p -> TIO.writeFile p ""

-- ------------------------------------------------------------ small text helpers

padR, padL :: Int -> Text -> Text
padR n = T.justifyLeft n ' '
padL n = T.justifyRight n ' '

firstOr :: a -> [a] -> a
firstOr d = fromMaybe d . listToMaybe

dieWith :: Text -> IO a
dieWith m = TIO.hPutStrLn stderr ("nixon: " <> m) >> exitFailure

-- ------------------------------------------------------------ running commands

isTTY :: IO Bool
isTTY = (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stdout

requireTTY :: IO ()
requireTTY = do
  t <- isTTY
  unless t $ dieWith "the interactive UI needs a terminal; see 'nixon --help' for scriptable commands"

shQuote :: String -> String
shQuote s
  | not (null s) && all safe s = s
  | otherwise = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) s ++ "'"
  where safe c = c `elem` ("-_./=:@%+,#^" :: String) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | Show a spinner while the action runs, but only on a terminal.
withSpinner :: Maybe Text -> IO a -> IO a
withSpinner Nothing act    = act
withSpinner (Just msg) act = do
  tty <- isTTY
  if tty then Gum.spin msg act else act

-- | Default spinner text: the command, shortened.
spinMsg :: [String] -> Text
spinMsg argv = "Running " <> short (showCmd argv) <> "…"
  where short t | T.length t > 60 = T.take 59 t <> "…"
                | otherwise      = t

showCmd :: [String] -> Text
showCmd = T.pack . unwords . map shQuote

-- | Spawn with inherited stdio. Ctrl-C is delegated to the child.
spawn :: [String] -> IO ExitCode
spawn [] = pure ExitSuccess
spawn (c : as) =
  ( do (_, _, _, ph) <- createProcess (proc c as) { delegate_ctlc = True }
       waitForProcess ph )
  `catch` \e -> do
    Gum.bad (T.pack (show (e :: IOException)))
    pure (ExitFailure 127)

-- | Run with some text piped to stdin.
spawnWithInput :: [String] -> Text -> IO ExitCode
spawnWithInput [] _ = pure ExitSuccess
spawnWithInput (c : as) input = do
  (mh, _, _, ph) <- createProcess (proc c as) { std_in = CreatePipe }
  forM_ mh $ \h -> hSetEncoding h utf8 >> TIO.hPutStr h input >> hClose h
  waitForProcess ph

-- | Print, log to history, run. True on exit 0.
runCmd :: [String] -> IO Bool
runCmd [] = pure True
runCmd argv = do
  Gum.dimErr ("$ " <> showCmd argv)
  record (showCmd argv)
  rc <- spawn argv
  case rc of
    ExitSuccess   -> pure True
    ExitFailure n -> Gum.bad ("exited with status " <> T.pack (show n)) >> pure False

-- | Like 'runCmd', but quiet: runs under a spinner and shows the output only
-- if the command fails. Not for interactive commands, since stdin is empty
-- and stdout/stderr are captured.
runCmdSpin :: Text -> [String] -> IO Bool
runCmdSpin _ [] = pure True
runCmdSpin msg argv = do
  Gum.dimErr ("$ " <> showCmd argv)
  record (showCmd argv)
  r <- withSpinner (Just msg) (readCmd argv)
  case r of
    Just (ExitSuccess, _, _) -> pure True
    Just (ExitFailure n, o, e) -> do
      Gum.bad ("exited with status " <> T.pack (show n))
      Gum.pager ("$ " <> showCmd argv <> "\n\n" <> o <> e)
      pure False
    Nothing -> Gum.bad "could not start the command" >> pure False

-- | Run a command and show its (stdout+stderr) output in the pager, without logging.
pagedCmd :: [String] -> IO ()
pagedCmd argv = pagedCmdWith (Just (spinMsg argv)) argv

pagedCmdWith :: Maybe Text -> [String] -> IO ()
pagedCmdWith spin argv = withSpinner spin (captureAll argv) >>= Gum.pager

-- | Like 'runCmd' but the output goes to the pager.
runPaged :: [String] -> IO ()
runPaged argv = runPagedWith (Just (spinMsg argv)) argv

runPagedWith :: Maybe Text -> [String] -> IO ()
runPagedWith spin argv = do
  record (showCmd argv)
  out <- withSpinner spin (captureAll argv)
  Gum.pager ("$ " <> showCmd argv <> "\n\n" <> out)

readCmd :: [String] -> IO (Maybe (ExitCode, Text, Text))
readCmd [] = pure Nothing
readCmd (c : as) = do
  r <- try (readProcessWithExitCode c as "")
  pure $ case r of
    Left (_ :: IOException) -> Nothing
    Right (ec, o, e)        -> Just (ec, T.pack o, T.pack e)

-- | stdout of a successful command (stderr discarded).
capture :: [String] -> IO (Maybe Text)
capture a = do
  r <- readCmd a
  pure $ case r of
    Just (ExitSuccess, o, _) -> Just o
    _                        -> Nothing

-- | stdout followed by stderr, regardless of exit status.
captureAll :: [String] -> IO Text
captureAll a = maybe "" (\(_, o, e) -> o <> e) <$> readCmd a

-- | Capture stdout but leave stderr attached to the terminal (so build progress stays visible).
captureOut :: [String] -> IO (Maybe Text)
captureOut [] = pure Nothing
captureOut (c : as) = do
  r <- try $ do
    (_, mo, _, ph) <- createProcess (proc c as) { std_out = CreatePipe }
    out <- maybe (pure "") TIO.hGetContents mo
    ec <- waitForProcess ph
    pure (ec, out)
  pure $ case r of
    Right (ExitSuccess, o)  -> Just o
    Right _                 -> Nothing
    Left (_ :: IOException) -> Nothing

have :: String -> IO Bool
have = fmap isJust . findExecutable

-- | Use the tool if installed, otherwise @nix shell CHANNEL#pkg -c tool@.
toolCmd :: String -> String -> [String] -> IO [String]
toolCmd tool pkg args = do
  h <- have tool
  if h then pure (tool : args) else do
    ch <- getChannel
    pure (["nix", "shell", T.unpack ch <> "#" <> pkg, "-c", tool] ++ args)

sudoPrefix :: IO [String]
sudoPrefix = do
  u <- getEffectiveUserID
  pure [ "sudo" | u /= 0 ]

toRef :: Text -> IO Text
toRef p
  | "#" `T.isInfixOf` p = pure p
  | otherwise           = (<> "#" <> p) <$> getChannel

-- | Bare names become CHANNEL#name; paths, flake refs and .#foo are left alone.
normalizeTarget :: Text -> IO Text
normalizeTarget t
  | any (`T.isPrefixOf` t) ["/", "."] || "#" `T.isInfixOf` t || ":" `T.isInfixOf` t = pure t
  | otherwise = toRef t

-- ------------------------------------------------------------ prompts

confirm :: Text -> IO Bool
confirm q = do
  y <- readIORef assumeYesRef
  if y then pure True else do
    tty <- isTTY
    if tty then Gum.confirm q else do
      TIO.putStr (q <> " [y/N] ")
      hFlush stdout
      l <- getLine `catch` \(_ :: IOException) -> pure ""
      pure (take 1 l `elem` ["y", "Y"])

pause :: IO ()
pause = do
  y <- readIORef assumeYesRef
  tty <- isTTY
  unless (y || not tty) Gum.pressAnyKey

ask :: Text -> Text -> IO (Maybe Text)
ask p ph = Gum.input (Gum.InputCfg p ph "")

askDef :: Text -> Text -> Text -> IO (Maybe Text)
askDef p ph v = Gum.input (Gum.InputCfg p ph v)

-- | 'ask', but an empty answer counts as cancelling too.
askNE :: Text -> Text -> IO (Maybe Text)
askNE p ph = mfilter (not . T.null) <$> ask p ph

-- ------------------------------------------------------------ menus

data Next = Stay | Leave

-- | A menu entry that returns to the menu afterwards.
opt :: Text -> IO () -> (Text, IO Next)
opt l a = (l, a >> pure Stay)

-- | A menu entry that closes the menu afterwards.
optLeave :: Text -> IO () -> (Text, IO Next)
optLeave l a = (l, a >> pure Leave)

-- | Generates the raw text lines for a rounded box, styled with cyan borders and white text.
banner :: Text -> Text -> Text
banner title ch = T.unlines $ build
  where
    rawNixon = "❄ NIXON " <> version
    rawChan  = "channel: " <> ch
    rawTitle = title
    cleanLines = filter (not . T.null) [rawNixon, rawChan, rawTitle]
    w = maximum (1 : map T.length cleanLines)
    renderLine l
      | "❄ NIXON" `T.isPrefixOf` l =
          let padded = T.justifyLeft w ' ' l
              (prefix, rest) = T.splitAt 8 padded
          in cyan <> prefix <> reset <> rest
      | "channel:" `T.isPrefixOf` l =
          let padded = T.justifyLeft w ' ' l
              (prefix, rest) = T.splitAt 9 padded
          in boldWhite <> prefix <> reset <> rest
      | otherwise =
          boldWhite <> T.justifyLeft w ' ' l <> reset
    build =
      let rows = map renderLine cleanLines
       in mconcat
            [ [ cyan <> "╭" <> T.replicate (w + 2) "─" <> "╮" <> reset ]
            , map (\r -> cyan <> "│ " <> reset <> r <> cyan <> " │" <> reset) rows
            , [ cyan <> "╰" <> T.replicate (w + 2) "─" <> "╯" <> reset ]
            ]
    cyan = "\ESC[36m"
    boldWhite = "\ESC[1;37m"
    reset = "\ESC[0m"

menuLoopWith :: Text -> Text -> [(Text, IO Next)] -> IO ()
menuLoopWith back hdr acts = go 0
  where
    go i = do
      r <- Gum.chooseAt i hdr opts
      case r of
        Just l | l /= back, Just a <- lookup l acts -> do
          n <- a
          case n of
            Stay -> go (fromMaybe 0 (elemIndex l opts))  -- save the row position
            Leave -> pure ()
        _ -> pure ()
    opts = map fst acts ++ [back]

menuLoop :: Text -> [(Text, IO Next)] -> IO ()
menuLoop title acts = do
  ch <- getChannel
  menuLoopWith "Back" (banner title ch) acts
