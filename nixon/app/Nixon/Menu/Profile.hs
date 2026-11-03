{-# LANGUAGE OverloadedStrings #-}
module Nixon.Menu.Profile (menuProfile) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Gum
import Nixon.Core
import Nixon.Json
import Nixon.Menu.Inspect (menuDiff)

align :: [(Text, Text)] -> [Text]
align es = [ padR (w + 2) k <> v | (k, v) <- es ]
  where w = maximum (0 : map (T.length . fst) es)

profileEntries :: IO [(Text, Text)]
profileEntries = do
  m <- capture ["nix", "profile", "list", "--json"]
  pure [ (k, fromMaybe "?" (listToMaybe (catMaybes [key "attrPath" v, key "originalUrl" v, key "url" v]) >>= strOf))
       | Just j <- [m >>= parse], Just els <- [key "elements" j], (k, v) <- members els ]

menuProfile :: IO ()
menuProfile = menuLoop "Profile"
  [ opt "List installed" $ do
      es <- profileEntries
      Gum.pager (T.unlines ("Installed in profile:" : "" : align es))
  , opt "Remove packages" $ do
      es <- profileEntries
      msel <- Gum.chooseMany Nothing "select packages to remove" (align es)
      let names = [ w | l <- fromMaybe [] msel, (w : _) <- [T.words l] ]
      unless (null names) $ do
        c <- confirm ("Remove " <> T.unwords names <> "?")
        when c (runCmd (["nix", "profile", "remove"] ++ map T.unpack names) >> pause)
  , opt "Upgrade all" $ do
      r <- runCmd ["nix", "profile", "upgrade", ".*"]
      unless r (void (runCmd ["nix", "profile", "upgrade", "--all"]))
      pause
  , opt "History" $ pagedCmd ["nix", "profile", "history"]
  , opt "Diff two generations" menuDiff
  , opt "Rollback" $ do
      c <- confirm "Roll back to the previous profile generation?"
      when c (runCmd ["nix", "profile", "rollback"] >> pause)
  ]
