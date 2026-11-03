{-# LANGUAGE OverloadedStrings #-}
-- | Wrapper of @fzf@ for lists too big for the built-in Brick filter.
--
-- Each 'Item' has a stable key, a display label, and an optional preview text.
-- Previews are written to a temp dir and shown with @cat@ so fzf never has to
-- re-run anything while you scroll.
module Gum.Fzf
  ( Item (..), item, plain
  , FzfOpts (..), fzfAvailable, fzfPick
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (forM_, void)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, hSetEncoding, utf8)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Text.Read (readMaybe)

data Item = Item
  { itemKey     :: Text
  , itemLabel   :: Text
  , itemPreview :: Maybe Text
  }

item :: Text -> Text -> Item
item k l = Item k l Nothing

-- | Key and label are the same string, no preview.
plain :: Text -> Item
plain t = Item t t Nothing

data FzfOpts = FzfOpts
  { fzHeader :: Text
  , fzMulti  :: Bool
  }

fzfAvailable :: IO Bool
fzfAvailable = isJust <$> findExecutable "fzf"

shq :: String -> String
shq s = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) s ++ "'"

flat :: Text -> Text
flat = T.map (\c -> if c == '\t' || c == '\n' || c == '\r' then ' ' else c)

-- | Run fzf. Returns @(typed query, selected items)@, or 'Nothing' if the user
-- aborted (Esc / Ctrl-C) or fzf failed.  Nothing selected + non-empty query
-- means "no match".
fzfPick :: FzfOpts -> [Item] -> IO (Maybe (Text, [Item]))
fzfPick o items = withSystemTempDirectory "gum-fzf" $ \dir -> do
  let indexed = zip [0 :: Int ..] items
      hasPreview = any (isJust . itemPreview) items
  forM_ indexed $ \(i, it) -> forM_ (itemPreview it) (TIO.writeFile (dir </> show i))
  let args =
        [ "--reverse", "--height=85%", "--border", "--header", T.unpack (fzHeader o)
        , "--delimiter", "\t", "--with-nth", "2..", "--print-query", "--tiebreak=index" ]
        ++ [ "--multi" | fzMulti o ]
        ++ (if hasPreview
              then [ "--preview", "cat " ++ shq dir ++ "/{1}", "--preview-window", "right,50%,wrap" ]
              else [])
      payload = T.unlines [ T.pack (show i) <> "\t" <> flat (itemLabel it) | (i, it) <- indexed ]
  (mhin, mhout, _, ph) <- createProcess (proc "fzf" args) { std_in = CreatePipe, std_out = CreatePipe }
  case (mhin, mhout) of
    (Just hin, Just hout) -> do
      hSetEncoding hin utf8
      hSetEncoding hout utf8
      -- feed stdin from a separate thread so a big list can't block against fzf's output
      void . forkIO $ void (try @IOException (TIO.hPutStr hin payload >> hClose hin))
      out  <- TIO.hGetContents hout
      code <- waitForProcess ph
      let (q, sel) = case T.lines out of
            []       -> ("", [])
            (x : xs) -> (x, xs)
          chosen = [ it | l <- sel
                        , Just i <- [readMaybe (T.unpack (T.takeWhile (/= '\t') l))]
                        , Just it <- [lookup i indexed] ]
      pure $ case code of
        ExitFailure 130 -> Nothing
        ExitFailure 2   -> Nothing
        _               -> Just (q, chosen)
    _ -> pure Nothing
