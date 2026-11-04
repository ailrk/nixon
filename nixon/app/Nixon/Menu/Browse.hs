{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.Browse where

import Nixon.Core
import Nixon.Browse.Menu.Inspect (menuInspect)
import Nixon.Browse.Menu.Tree (menuTree)


-- browse tool ------------------------------------------------------------

menuBrowse :: IO ()
menuBrowse = menuLoop "Browse Tools"
  [ opt "Inspect Config" menuInspect
  , opt "Inspect dependencies" menuTree
  ]
