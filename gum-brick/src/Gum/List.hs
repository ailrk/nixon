{-# LANGUAGE OverloadedStrings #-}
-- | Meant for small lists (menus, a few hundred entries).  For big lists use
-- "Gum.Pick", which hands the work to fzf when available.
module Gum.List
  ( ListCfg (..), defaultList, runList
  , choose, chooseMany, filterOne, filterMany, filterFree
  ) where

import Brick
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Gum.Internal

data ListCfg = ListCfg
  { lcHeader      :: Text
  , lcHeight      :: Int          -- ^ visible rows
  , lcMulti       :: Bool         -- ^ allow selecting several items
  , lcLimit       :: Maybe Int    -- ^ max number of selections (multi only)
  , lcFilter      :: Bool         -- ^ type-to-filter line
  , lcFree        :: Bool         -- ^ Enter with no match returns the typed text (gum's --no-strict)
  , lcPlaceholder :: Text
  }

defaultList :: ListCfg
defaultList = ListCfg "" 12 False Nothing False False "filter…"

data St = St
  { sCfg   :: ListCfg
  , sItems :: [(Int, Text)]
  , sQuery :: Text
  , sCur   :: Int           -- ^ index into the *visible* list
  , sOff   :: Int           -- ^ first visible row shown
  , sSel   :: [Int]         -- ^ selected item ids, in selection order
  , sRes   :: Maybe [Text]
  }

-- | Run the list. 'Nothing' means the user cancelled.
runList :: ListCfg -> [Text] -> IO (Maybe [Text])
runList cfg xs = sRes <$> defaultMain app (St cfg (zip [0 ..] xs) "" 0 0 [] Nothing)
  where
    app = App
      { appDraw         = draw
      , appChooseCursor = neverShowCursor
      , appHandleEvent  = handle
      , appStartEvent   = pure ()
      , appAttrMap      = const theme
      }

matches :: Text -> Text -> Bool
matches q t = all (`T.isInfixOf` T.toLower t) (T.words (T.toLower q))

visible' :: St -> [(Int, Text)]
visible' s = [ it | it@(_, t) <- sItems s, matches (sQuery s) t ]

move :: Int -> EventM Res St ()
move d = modify $ \s ->
  let n = length (visible' s)
      h = lcHeight (sCfg s)
      c = if n == 0 then 0 else max 0 (min (n - 1) (sCur s + d))
      o | c < sOff s       = c
        | c >= sOff s + h  = c - h + 1
        | otherwise        = sOff s
  in s { sCur = c, sOff = o }

setQuery :: Text -> EventM Res St ()
setQuery q = modify $ \s -> s { sQuery = q, sCur = 0, sOff = 0 }

toggle :: EventM Res St ()
toggle = modify $ \s -> case drop (sCur s) (visible' s) of
  ((i, _) : _)
    | i `elem` sSel s -> s { sSel = filter (/= i) (sSel s) }
    | maybe True (length (sSel s) <) (lcLimit (sCfg s)) -> s { sSel = sSel s ++ [i] }
  _ -> s

finish :: [Text] -> EventM Res St ()
finish xs = modify (\s -> s { sRes = Just xs }) >> halt

submit :: EventM Res St ()
submit = do
  s <- get
  let vis = visible' s
      picked = [ t | i <- sSel s, Just t <- [lookup i (sItems s)] ]
  if lcMulti (sCfg s) && not (null picked)
    then finish picked
    else case drop (sCur s) vis of
      ((_, t) : _) -> finish [t]
      []           | lcFree (sCfg s) && not (T.null (sQuery s)) -> finish [sQuery s]
                   | otherwise -> pure ()

handle :: BrickEvent Res () -> EventM Res St ()
handle (VtyEvent (V.EvKey k ms)) = do
  s <- get
  let cfg   = sCfg s
      filt  = lcFilter cfg
      multi = lcMulti cfg
      h     = lcHeight cfg
  case (k, ms) of
    (V.KEsc, _)                -> halt
    (V.KChar 'c', [V.MCtrl])   -> halt
    (V.KEnter, _)              -> submit
    (V.KChar 'g', [])          -> move (-1000000)
    (V.KChar 'G', _)           -> move 1000000
    (V.KHome, _)               -> move (-1000000)
    (V.KEnd, _)                -> move 1000000
    (V.KUp, _)                 -> move (-1)
    (V.KDown, _)               -> move 1
    (V.KChar 'p', [V.MCtrl])   -> move (-1)
    (V.KChar 'n', [V.MCtrl])   -> move 1
    (V.KPageUp, _)             -> move (negate (h `div` 2))
    (V.KPageDown, _)           -> move (h `div` 2)
    (V.KChar 'u', [V.MCtrl])   -> move (negate (h `div` 2))
    (V.KChar 'd', [V.MCtrl])   -> move (h `div` 2)
    (V.KChar '\t', _)        | multi  -> toggle >> move 1
    (V.KChar ' ', [])        | multi && not filt -> toggle
    (V.KChar 'j', [])        | not filt -> move 1
    (V.KChar 'k', [])        | not filt -> move (-1)
    (V.KChar 'q', [])        | not filt -> halt
    (V.KBS, _)               | filt -> setQuery (T.dropEnd 1 (sQuery s))
    (V.KChar c, [])          | filt -> setQuery (sQuery s <> T.singleton c)
    _ -> pure ()
handle _ = pure ()

draw :: St -> [Widget Res]
draw s = [ padLeftRight 1 (vBox (hdr ++ qline ++ body ++ [foot])) ]
  where
    cfg = sCfg s
    vis = visible' s
    h   = lcHeight cfg
    hdr = [ withAttr aAccent (txtS (lcHeader cfg)) | not (T.null (lcHeader cfg)) ]
    qline
      | lcFilter cfg =
          [ hBox [ withAttr aAccent (txtS "▸ ")
                 , if T.null (sQuery s)
                     then withAttr aDim (txtS (lcPlaceholder cfg))
                     else txtS (sQuery s)
                 , withAttr aSel (txtS " ") ] ]
      | otherwise = []
    body
      | null vis  = [ withAttr aDim (txtS "  no matches") ]
      | otherwise = zipWith row [sOff s ..] (take h (drop (sOff s) vis))
    row pos (i, t) =
      let isCur = pos == sCur s
          mark | not (lcMulti cfg) = ""
               | i `elem` sSel s   = "✓ "
               | otherwise         = "  "
          prefix = if isCur then "▸ " else "  "
      in (if isCur then withAttr aSel else id) (txtS (prefix <> mark <> t))
    foot = withAttr aDim (txtS (T.pack (show (length vis)) <> "/" <> T.pack (show (length (sItems s))) <> " · " <> help))
    help | lcMulti cfg = "tab/space: toggle · enter: confirm · g/G top/bottom · esc: cancel"
         | otherwise   = "enter: select · g/G top/bottom · esc: cancel"

-- wrappers ----------------------------------------------------------------

clampH :: Int -> [a] -> Int
clampH m xs = max 1 (min m (length xs))

choose :: Text -> [Text] -> IO (Maybe Text)
choose hdr xs = (>>= listToMaybe) <$> runList defaultList { lcHeader = hdr, lcHeight = clampH 14 xs } xs

chooseMany :: Maybe Int -> Text -> [Text] -> IO (Maybe [Text])
chooseMany lim hdr xs =
  runList defaultList { lcHeader = hdr, lcHeight = clampH 14 xs, lcMulti = True, lcLimit = lim } xs

filterOne :: Text -> [Text] -> IO (Maybe Text)
filterOne hdr xs = (>>= listToMaybe) <$> runList defaultList { lcHeader = hdr, lcHeight = 18, lcFilter = True } xs

filterMany :: Maybe Int -> Text -> [Text] -> IO (Maybe [Text])
filterMany lim hdr xs =
  runList defaultList { lcHeader = hdr, lcHeight = 18, lcFilter = True, lcMulti = True, lcLimit = lim } xs

filterFree :: Text -> [Text] -> IO (Maybe Text)
filterFree hdr xs =
  (>>= listToMaybe) <$> runList defaultList
    { lcHeader = hdr, lcHeight = 18, lcFilter = True, lcFree = True
    , lcPlaceholder = "type to filter, or type a target…" } xs
