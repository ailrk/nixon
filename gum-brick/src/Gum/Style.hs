{-# LANGUAGE OverloadedStrings #-}
-- | Plain-terminal output helpers (the @gum style@ part).  No Brick involved.
module Gum.Style
  ( say, ok, warn, bad, dim, dimErr, clearTerminal, pressAnyKey) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Word (Word8)
import qualified System.Console.ANSI as A
import System.IO

-- | Print a line in a 256-colour palette colour (plain when not a terminal).
say :: Handle -> Word8 -> Text -> IO ()
say h c t = do
  col <- A.hSupportsANSI h
  when col $ A.hSetSGR h [A.SetPaletteColor A.Foreground c]
  TIO.hPutStrLn h t
  when col $ A.hSetSGR h [A.Reset]

ok, dim :: Text -> IO ()
ok  = say stdout 42 . ("✓ " <>)
dim = say stdout 245

warn, bad, dimErr :: Text -> IO ()
warn   = say stderr 214 . ("! " <>)
bad    = say stderr 196 . ("✗ " <>)
dimErr = say stderr 245

clearTerminal :: IO ()
clearTerminal = do
  col <- A.hSupportsANSI stdout
  when col $ A.clearScreen >> A.setCursorPosition 0 0

pressAnyKey :: IO ()
pressAnyKey = do
  tty <- hIsTerminalDevice stdin
  when tty $ do
    TIO.putStr "\npress any key to continue…"
    hFlush stdout
    buf  <- hGetBuffering stdin
    echo <- hGetEcho stdin
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    (getChar >> pure ()) `finally` (hSetBuffering stdin buf >> hSetEcho stdin echo)
    putStrLn ""
