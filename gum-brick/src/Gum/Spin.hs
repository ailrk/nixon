{-# LANGUAGE OverloadedStrings #-}
-- | @gum spin@: show a spinner while an IO action runs on a worker thread
-- and hand back its result (exceptions are re-thrown on the caller).
module Gum.Spin (spin) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Data.Text (Text)
import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Gum.Internal

data Ev = Tick | Done
data SS = SS Text Int

frames :: [Text]
frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

app :: App SS Ev Res
app = App
  { appDraw         = \(SS t n) ->
      [ padLeftRight 1 (hBox [ withAttr aAccent (txtS (frames !! (n `mod` length frames)))
                             , txtS (" " <> t) ]) ]
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handle
  , appStartEvent   = pure ()
  , appAttrMap      = const theme
  }
  where
    handle :: BrickEvent Res Ev -> EventM Res SS ()
    handle (AppEvent Tick) = modify (\(SS t n) -> SS t (n + 1))
    handle (AppEvent Done) = halt
    handle (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt
    handle _ = pure ()

-- | Ctrl-C while spinning abandons the action and throws 'UserInterrupt'.
spin :: forall a. Text -> IO a -> IO a
spin title act = do
  box  <- newEmptyMVar
  chan <- newBChan 16
  worker <- forkIO $ do
    r <- try act
    putMVar box (r :: Either SomeException a)
    writeBChan chan Done
  ticker <- forkIO $ forever (threadDelay 100000 >> writeBChan chan Tick)
  let build = mkVty defaultConfig
  v <- build
  _ <- customMain v build (Just chan) app (SS title 0)
  killThread ticker
  r <- tryTakeMVar box
  case r of
    Nothing        -> killThread worker >> throwIO UserInterrupt
    Just (Left e)  -> throwIO e
    Just (Right x) -> pure x
