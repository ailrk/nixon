{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.Options (menuOptions) where

import Control.Exception (evaluate)
import Control.Monad
import Data.Maybe
import Gum (Item (..), bad)
import Nixon.Core
import Nixon.Json
import Nixon.Nix
import qualified Data.Text.IO as TIO
import qualified Gum

menuOptions :: IO ()
menuOptions = do
  mk <- Gum.choose "Option set" ["NixOS", "Home Manager"]
  forM_ mk $ \k -> do
    loaded <- Gum.spin ("Loading " <> k <> " options…") $ do
      r <- optionsJson (if k == "NixOS" then OptNixos else OptHm)
      case r of
        Left e     -> pure (Left e)
        Right json -> do
          mv <- parse <$> TIO.readFile json
          _  <- evaluate (maybe 0 (length . members) mv) -- force the parse
          pure (maybe (Left "could not parse the option index") Right mv)
    case loaded of
      Left e  -> bad e >> pause
      Right v -> do
        let items = [ Item n (padR 64 n <> " " <> fromMaybe "" (key "type" o >>= strOf)) (Just (optionDetail n o))
                    | (n, o) <- members v ]
            hdr = "search " <> k <> " options · enter: details · esc: back"
            loop = do
              sel <- Gum.pickOne hdr items
              forM_ sel $ \it -> do
                forM_ (lookup (itemKey it) (members v)) $ \o -> Gum.pager (optionDetail (itemKey it) o)
                loop
        loop
