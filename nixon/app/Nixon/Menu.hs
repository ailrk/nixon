{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu (menu) where

import Nixon.Menu.Admin qualified as Menu.Admin
import Nixon.Menu.Flake qualified as Menu.Flake
import Nixon.Menu.Packages qualified as Menu.Packages
import Nixon.Menu.System qualified as Menu.System
import Nixon.Menu.Inspect qualified as Menu.Inspect
import Nixon.Menu.Profile qualified as Menu.Profile
import Nixon.Menu.Options qualified as Menu.Options
import Nixon.Menu.Browse qualified as Menu.Browse
import System.Info (os)
import Nixon.Core (menuLoop, opt)

isMac :: Bool
isMac = os == "darwin"

menu :: IO ()
menu = do
  menuLoop "" $ mconcat
    [ [ opt "Flake" $ Menu.Flake.menuFlake
      , opt "Profile" $ Menu.Profile.menuProfile
      , opt "Packages" $ Menu.Packages.menuPackages
      , opt "Inspect" $ Menu.Inspect.menuInspect
      , opt "Channels" $ Menu.Admin.menuChannels
      , opt "Caches" $ Menu.Admin.menuCaches
      , opt "Maintenance" $ Menu.Admin.menuMaintenance
      , opt "Browse" $ Menu.Browse.menuBrowse
      , opt "Options" $ Menu.Options.menuOptions
      , opt "History" $ Menu.Packages.menuHistory
      , opt "NixOS" $ Menu.System.menuNixos
      , opt "Home Mananger" $ Menu.System.menuHomeManager
      ]
      , if isMac then [ opt "Darwin" $ Menu.System.menuDarwin ] else []
    ]
