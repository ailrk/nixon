{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.Options (menuOptions) where

import Control.Monad
import Data.Maybe
import qualified Data.Text.IO as TIO
import Gum (Item (..), bad)
import qualified Gum
import Nixon.Core
import Nixon.Json
import Nixon.Nix

menuOptions :: IO ()
menuOptions = do
  mk <- Gum.choose "Option set" ["NixOS", "Home Manager"]
  forM_ mk $ \k -> do
    r <- optionsJson (if k == "NixOS" then OptNixos else OptHm)
    case r of
      Left e -> bad e >> pause
      Right json -> do
        mv <- parse <$> TIO.readFile json
        case mv of
          Nothing -> bad "could not parse the option index" >> pause
          Just v -> do
            let items = [ Item n (padR 64 n <> " " <> fromMaybe "" (key "type" o >>= strOf)) (Just (optionDetail n o))
                        | (n, o) <- members v ]
                hdr = "search " <> k <> " options · enter: details · esc: back"
                loop = do
                  sel <- Gum.pickOne hdr items
                  forM_ sel $ \it -> do
                    forM_ (lookup (itemKey it) (members v)) $ \o -> Gum.pager (optionDetail (itemKey it) o)
                    loop
            loop
