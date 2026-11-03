{-# LANGUAGE OverloadedStrings #-}
-- | @gum input@ and @gum confirm@ on Brick.
module Gum.Prompt (InputCfg (..), input, confirm) where

import Brick
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Gum.Internal

data InputCfg = InputCfg
  { icPrompt      :: Text
  , icPlaceholder :: Text
  , icValue       :: Text
  }

data IS = IS { isText :: Text, isRes :: Maybe Text }

-- | 'Nothing' when cancelled with Esc / Ctrl-C; 'Just ""' for an empty Enter.
input :: InputCfg -> IO (Maybe Text)
input cfg = isRes <$> defaultMain app (IS (icValue cfg) Nothing)
  where
    app = App
      { appDraw         = draw
      , appChooseCursor = neverShowCursor
      , appHandleEvent  = handle
      , appStartEvent   = pure ()
      , appAttrMap      = const theme
      }
    draw s =
      [ padLeftRight 1 $ vBox
          [ hBox [ withAttr aAccent (txtS (icPrompt cfg <> " ▸ ")), body s ]
          , withAttr aDim (txtS "enter: accept · esc: cancel") ] ]
    body s
      | T.null (isText s) = hBox [ withAttr aSel (txtS " "), withAttr aDim (txtS (icPlaceholder cfg)) ]
      | otherwise         = hBox [ txtS (isText s), withAttr aSel (txtS " ") ]
    handle :: BrickEvent Res () -> EventM Res IS ()
    handle (VtyEvent (V.EvKey k ms)) = case (k, ms) of
      (V.KEsc, _)              -> halt
      (V.KChar 'c', [V.MCtrl]) -> halt
      (V.KEnter, _)            -> modify (\s -> s { isRes = Just (isText s) }) >> halt
      (V.KBS, _)               -> modify (\s -> s { isText = T.dropEnd 1 (isText s) })
      (V.KChar 'u', [V.MCtrl]) -> modify (\s -> s { isText = "" })
      (V.KChar 'w', [V.MCtrl]) -> modify (\s -> s { isText = T.dropWhileEnd (/= ' ') (T.dropWhileEnd (== ' ') (isText s)) })
      (V.KChar c, [])          -> modify (\s -> s { isText = isText s <> T.singleton c })
      _                        -> pure ()
    handle _ = pure ()

data CS = CS { csYes :: Bool, csRes :: Maybe Bool }

-- | Yes/No prompt; defaults to "No" so a stray Enter never confirms something destructive.
confirm :: Text -> IO Bool
confirm q = maybe False id . csRes <$> defaultMain app (CS False Nothing)
  where
    app = App
      { appDraw         = draw
      , appChooseCursor = neverShowCursor
      , appHandleEvent  = handle
      , appStartEvent   = pure ()
      , appAttrMap      = const theme
      }
    draw s =
      [ padLeftRight 1 $ vBox
          [ withAttr aAccent (txtS q)
          , txtS " "
          , hBox [ btn (csYes s) " Yes ", txtS "  ", btn (not (csYes s)) " No " ]
          , withAttr aDim (txtS "y/n · ←/→ · enter") ] ]
    btn on l = (if on then withAttr aSel else withAttr aDim) (txtS ("[" <> l <> "]"))
    answer b = modify (\s -> s { csRes = Just b }) >> halt
    handle :: BrickEvent Res () -> EventM Res CS ()
    handle (VtyEvent (V.EvKey k ms)) = case (k, ms) of
      (V.KChar 'y', _)         -> answer True
      (V.KChar 'Y', _)         -> answer True
      (V.KChar 'n', _)         -> answer False
      (V.KChar 'N', _)         -> answer False
      (V.KEsc, _)              -> answer False
      (V.KChar 'c', [V.MCtrl]) -> answer False
      (V.KEnter, _)            -> modify (\s -> s { csRes = Just (csYes s) }) >> halt
      (V.KLeft, _)             -> flipSel
      (V.KRight, _)            -> flipSel
      (V.KChar '\t', _)        -> flipSel
      (V.KChar 'h', [])        -> flipSel
      (V.KChar 'l', [])        -> flipSel
      _                        -> pure ()
    handle _ = pure ()
    flipSel = modify (\s -> s { csYes = not (csYes s) })
