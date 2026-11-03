{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Nixon.Core qualified as Core
import System.Environment (getArgs)
import Nixon.Cli (usage, cli)
import Nixon.Menu (menu)

main :: IO ()
main = do
  Core.initState
  args <- getArgs
  if length args == 0
     then menu
     else case args of
            ["--help"] -> usage
            ["-h"]     -> usage
            _          -> cli args
