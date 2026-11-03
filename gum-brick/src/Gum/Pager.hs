{-# LANGUAGE OverloadedStrings #-}
-- | @gum pager@: scroll through text with j/k, space/b, g/G, q.
module Gum.Pager (pager) where

import Brick
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Gum.Internal

pager :: Text -> IO ()
pager t = case T.lines t of
  []  -> pure ()
  ls  -> void (defaultMain app ls)
  where
    app :: App [Text] () Res
    app = App
      { appDraw         = draw
      , appChooseCursor = neverShowCursor
      , appHandleEvent  = handle
      , appStartEvent   = pure ()
      , appAttrMap      = const theme
      }
    vp = viewportScroll NView
    draw ls =
      [ vBox
          [ viewport NView Vertical (padLeftRight 1 (vBox (map txtS ls)))
          , withAttr aDim (txtS " j/k ↑/↓ scroll · space/b page · g/G top/bottom · q quit") ] ]
    handle :: BrickEvent Res () -> EventM Res [Text] ()
    handle (VtyEvent (V.EvKey k ms)) = case (k, ms) of
      (V.KChar 'q', _)         -> halt
      (V.KEsc, _)              -> halt
      (V.KEnter, _)            -> halt
      (V.KChar 'c', [V.MCtrl]) -> halt
      (V.KChar 'j', [])        -> vScrollBy vp 1
      (V.KDown, _)             -> vScrollBy vp 1
      (V.KChar 'k', [])        -> vScrollBy vp (-1)
      (V.KUp, _)               -> vScrollBy vp (-1)
      (V.KChar ' ', [])        -> vScrollPage vp Down
      (V.KPageDown, _)         -> vScrollPage vp Down
      (V.KChar 'b', [])        -> vScrollPage vp Up
      (V.KPageUp, _)           -> vScrollPage vp Up
      (V.KChar 'g', [])        -> vScrollToBeginning vp
      (V.KHome, _)             -> vScrollToBeginning vp
      (V.KChar 'G', _)         -> vScrollToEnd vp
      (V.KEnd, _)              -> vScrollToEnd vp
      _                        -> pure ()
    handle _ = pure ()
