{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | nix-inspect style browsing: one long-lived `nix repl` holds the evaluator,
-- so values forced earlier are reused. No menus here.
module Nixon.Browse.Inspect
  ( Kind (..), Node (..)
  , Repl
  , rootExpr
  , withRepl
  , replInspect
  , nixStr
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nixon.Browse.Core
import Nixon.Json
import System.IO
import System.Process

data Kind = KSet | KList | KLeaf | KDrv deriving (Eq, Show)

-- | One navigation step. Children are bare names: nothing below this level has
-- been forced, so huge or partly failing sets stay cheap and safe.
data Node = Node
  { ndKind     :: Kind
  , ndType     :: Text          -- Nix typeOf
  , ndValue    :: Maybe Text    -- rendered value, for leaves
  , ndChildren :: [Text]        -- attribute names, or "0".."n-1" for lists
  }

data Repl = Repl
  { _rIn   :: Handle
  , _rOut  :: Handle
  , _rPh   :: ProcessHandle
  , _rLock :: MVar ()            -- one request at a time
  }

sentinel :: Text
sentinel = "__NIXON_END__"

-- ------------------------------------------------------------ Nix text helpers

-- | A Haskell string as a Nix string literal.
nixStr :: Text -> Text
nixStr s = "\"" <> T.concatMap esc s <> "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '$'  = "\\$"
    esc '\n' = "\\n"
    esc c    = T.singleton c

-- | Undo the escaping `nix repl` applies when it prints a string value.
unNixStr :: Text -> Text
unNixStr = T.pack . go . T.unpack . T.dropEnd 1 . T.drop 1
  where
    go ('\\' : 'n' : r) = '\n' : go r
    go ('\\' : 't' : r) = '\t' : go r
    go ('\\' : 'r' : r) = '\r' : go r
    go ('\\' : c : r)   = c : go r
    go (c : r)          = c : go r
    go []               = []

-- | What to bind to `root`. A leading "expr:" means a raw Nix expression;
-- anything else is a flake reference or local flake path.
rootExpr :: Text -> IO Text
rootExpr t = case T.stripPrefix "expr:" t of
  Just e  -> pure e
  Nothing -> do
    r <- absolutize t
    pure ("builtins.getFlake " <> nixStr r)

-- | Defined once in the repl. Lists children by name only (no per-child
-- forcing), auto-calls functions whose arguments all have defaults.
inspectFn :: Text
inspectFn = T.unwords
  [ "nixonInspect = root: path:"
  , "let"
  , "  call = x: if builtins.isFunction x && builtins.all (d: d) (builtins.attrValues (builtins.functionArgs x)) then x {} else x;"
  , "  step = a: k: let b = call a; in if builtins.isList b then builtins.elemAt b (builtins.fromJSON k) else b.${k};"
  , "  v = call (builtins.foldl' step root path);"
  , "  t = builtins.typeOf v;"
  , "  isDrv = (builtins.tryEval ((v.type or null) == \"derivation\")).value;"
  , "in"
  , "  if t == \"set\" then { kind = if isDrv then \"drv\" else \"set\"; type = t; data = builtins.attrNames v; }"
  , "  else if t == \"list\" then { kind = \"list\"; type = t; data = builtins.length v; }"
  , "  else if t == \"lambda\" || t == \"primop\" then { kind = \"leaf\"; type = t; data = null; }"
  , "  else if t == \"path\" then { kind = \"leaf\"; type = t; data = toString v; }"
  , "  else { kind = \"leaf\"; type = t; data = v; }"
  ]

-- ------------------------------------------------------------ the repl process

-- | Start a repl bound to @root@, run the action, always stop the repl.
withRepl :: Text -> (Repl -> IO a) -> IO (Either Text a)
withRepl root act = do
  er <- start
  case er of
    Left e  -> pure (Left e)
    Right r -> Right <$> act r `finally` stop r
  where
    start = do
      (rd, wr) <- createPipe
      r <- try $ createProcess (proc "nix" ["repl", "--extra-experimental-features", "nix-command flakes"])
                   { std_in = CreatePipe, std_out = UseHandle wr, std_err = UseHandle wr }
      case r of
        Left (e :: IOException) -> pure (Left (T.pack (show e)))
        Right (Just hin, _, _, ph) -> do
          hSetEncoding hin utf8
          hSetEncoding rd utf8
          hSetBuffering hin LineBuffering
          lock <- newMVar ()
          let repl = Repl hin rd ph lock
          a <- raw repl ("root = " <> root)
          b <- raw repl inspectFn
          pure $ case (a, b) of
            (Right o, _) | isErr o -> Left (T.strip o)
            (_, Right o) | isErr o -> Left (T.strip o)
            (Left e, _)            -> Left e
            (_, Left e)            -> Left e
            _                      -> Right repl
        Right _ -> pure (Left "could not open pipes to nix repl")
    isErr = ("error:" `T.isInfixOf`)
    stop (Repl hin _ ph _) = do
      _ <- try (hClose hin) :: IO (Either IOException ())
      terminateProcess ph
      void (waitForProcess ph)

-- | Send one line, collect everything up to the sentinel echo.
raw :: Repl -> Text -> IO (Either Text Text)
raw (Repl hin hout _ lock) l = withMVar lock $ \_ -> do
  r <- try $ do
    TIO.hPutStrLn hin l
    TIO.hPutStrLn hin (nixStr sentinel)
    hFlush hin
    loop []
  pure $ case r of
    Left (e :: IOException) -> Left ("evaluator stopped: " <> T.pack (show e))
    Right o                 -> o
  where
    loop acc = do
      eof <- hIsEOF hout
      if eof then pure (Left "evaluator exited") else do
        ln <- TIO.hGetLine hout
        let ln' = fromMaybe ln (T.stripPrefix "nix-repl> " ln)
        if sentinel `T.isInfixOf` ln'
          then pure (Right (T.unlines (reverse acc)))
          else loop (ln' : acc)

-- | One navigation step: evaluate @path@ under @root@.
replInspect :: Repl -> [Text] -> IO (Either Text Node)
replInspect r path = do
  out <- raw r ("builtins.toJSON (nixonInspect root [ " <> T.unwords (map nixStr path) <> " ])")
  pure $ do
    o <- out
    -- the result is the one line that is a printed string; anything else is an error
    case filter ("\"" `T.isPrefixOf`) (T.lines o) of
      (l : _) -> maybe (Left "could not read evaluator output") Right (parse (unNixStr l) >>= toNode)
      []      -> Left (let e = T.strip o in if T.null e then "no output from evaluator" else e)

toNode :: Value -> Maybe Node
toNode v = do
  k <- key "kind" v >>= strOf
  let d     = key "data" v
      kind  = case k of "set" -> KSet; "list" -> KList; "drv" -> KDrv; _ -> KLeaf
      names = case kind of
        KList -> [ T.pack (show i) | Just n <- [d >>= intOf], i <- [0 .. n - 1] ]
        KLeaf -> []
        _     -> strsOf d
      val | kind /= KLeaf = Nothing
          | otherwise     = Just $ case d of
              Nothing   -> "null"
              Just Null -> "null"
              Just x    -> fromMaybe (pretty x) (strOf x)
  pure (Node kind (fromMaybe "" (key "type" v >>= strOf)) val names)
