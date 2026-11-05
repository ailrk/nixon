{-# LANGUAGE OverloadedStrings #-}
-- | Full-screen navigators.
--
-- - 'browseTree': (parent | current | preview) over anything shaped like a
-- tree. Nodes are loaded lazily on a worker thread, one level ahead of the
-- cursor, so the UI never blocks.
--
-- - 'browseGraph': referrers | dependencies | details over a directed graph,
-- with history, sorting by any column, and filtering.
module Gum.Browser
  ( -- Tree browser
    TreeNode (..)
  , TreeCfg (..)
  , treeCfg
  , browseTree
    -- Graph browser
  , Column (..)
  , Detail (..)
  , GraphCfg (..)
  , browseGraph
  ) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (vBorder)
import Control.Concurrent
import Control.Exception (SomeException, finally, try)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (elemIndex, nub, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Gum.Internal (aAccent, aDim, sanitize, txtS)
import Lens.Micro ((^.))

-- public types ------------------------------------------------------------

-- | What a loader returns for one path.
data TreeNode
  = Branch
      { tnType     :: Text     -- ^ shown in the preview header (e.g. "set")
      , tnBadge    :: Text     -- ^ shown after the name once visited ("/", " [12]")
      , tnChildren :: [Text]   -- ^ child names; descend with @path ++ [name]@
      }
  | Leaf
      { tnType  :: Text
      , tnValue :: Text        -- ^ rendered value (may be multi-line)
      }

data TreeCfg = TreeCfg
  { tcTitle :: Text                                   -- ^ title bar, e.g. "inspect"
  , tcLabel :: Text                                   -- ^ what is being browsed
  , tcLoad  :: [Text] -> IO (Either Text TreeNode)    -- ^ called from one worker thread
  , tcSplit :: Text -> [Text]                         -- ^ jump prompt: text to path
  , tcSep   :: Text                                   -- ^ joins a path for display
  }

-- | Dot-separated paths.
treeCfg :: Text -> Text -> ([Text] -> IO (Either Text TreeNode)) -> TreeCfg
treeCfg title label load =
  TreeCfg title label load (filter (not . T.null) . T.splitOn ".") "."

-- | A column in the graph lists. Items are identified by a key.
data Column = Column
  { colName  :: Text
  , colWidth :: Int
  , colCell  :: Text -> Text                          -- ^ key to cell text
  , colOrder :: Maybe (Text -> Text -> Ordering)      -- ^ sortable if given
  }

-- | A line in the details pane.
data Detail
  = DHead Text          -- ^ section title
  | DField Text Text    -- ^ label and value
  | DText Text          -- ^ wrapped text
  | DAccent Text        -- ^ wrapped, highlighted
  | DDim Text           -- ^ wrapped, dim
  | DLine Text          -- ^ one unwrapped line (keeps leading spaces)
  | DGap

data GraphCfg = GraphCfg
  { gcTitle   :: Text
  , gcStart   :: Text                     -- ^ key of the initial item
  , gcLabel   :: Text -> Text             -- ^ display name for a key
  , gcDeps    :: Text -> [Text]
  , gcRefs    :: Text -> [Text]
  , gcColumns :: [Column]
  , gcSummary :: Text -> Text             -- ^ title bar text for the current item
  , gcDetails :: Text -> [Detail]         -- ^ right pane for the highlighted item
  }

-- local definitions ------------------------------------------------------------

data Name = NName deriving (Eq, Ord, Show)

aRowSel, aRowSelDim, aTitle, aErr, aSet :: AttrName
aRowSel    = attrName "gum-row-sel"
aRowSelDim = attrName "gum-row-seldim"
aTitle     = attrName "gum-title"
aErr       = attrName "gum-err"
aSet       = attrName "gum-set"

browserTheme :: AttrMap
browserTheme = attrMap V.defAttr
  [ (aRowSel,    V.withStyle (V.black `on` V.cyan) V.bold)
  , (aRowSelDim, V.withStyle (fg V.cyan) V.underline)
  , (aDim,       V.withStyle (fg V.white) V.dim)
  , (aAccent,    fg V.cyan)
  , (aTitle,     V.withStyle (fg V.cyan) V.bold)
  , (aErr,       fg V.red)
  , (aSet,       fg V.blue)
  ]

wrapS :: Text -> Widget n
wrapS t = vBox (map (txtWrap . (\l -> if T.null l then " " else sanitize l)) (T.lines t))

dimW, errW, hdrW :: Text -> Widget n
dimW = withAttr aDim . txtS
errW = withAttr aErr . wrapS . T.unlines . take 30 . T.lines
hdrW = withAttr aTitle . txtS

-- | Only the visible rows are laid out; the window follows the cursor.
windowList :: Maybe Int -> [Widget Name] -> Widget Name
windowList mcur rows = Widget Greedy Greedy $ do
  c <- getContext
  let h   = max 1 (c ^. availHeightL)
      n   = length rows
      cur = fromMaybe 0 mcur
      top = max 0 (min (n - h) (cur - h `div` 2))
  render (vLimit h (padBottom Max (vBox (take h (drop top rows)))))

-- | Build the row plain when it is the cursor row, or colours get mixed.
selRow :: Bool -> Bool -> Widget n -> Widget n
selRow isCur focused w
  | isCur     = withAttr (if focused then aRowSel else aRowSelDim) (padRight Max w)
  | otherwise = w

editLine :: V.Key -> [V.Modifier] -> Text -> Maybe Text
editLine k ms t = case k of
  V.KBS                           -> Just (T.dropEnd 1 t)
  V.KChar 'u' | V.MCtrl `elem` ms -> Just ""
  V.KChar c   | V.MCtrl `notElem` ms, V.MMeta `notElem` ms -> Just (t <> T.singleton c)
  _                               -> Nothing

halfPage :: Int
halfPage = 10

spinnerFrame :: Int -> Text
spinnerFrame n = frames !! (n `mod` length frames)
  where frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

startTicker :: BChan e -> e -> IO ThreadId
startTicker chan ev = forkIO (forever (threadDelay 100000 >> writeBChan chan ev))

runApp :: Ord n => Maybe (BChan e) -> App s e n -> s -> IO s
runApp mchan app st = do
  let build = mkVty defaultConfig
  v <- build
  customMain v build mchan app st

cursorMark :: Widget n
cursorMark = withAttr aAccent (txtS "▌")

column :: Text -> Widget Name -> Widget Name
column title body =
  vBox [ padLeftRight 1 (withAttr aTitle (txtS (if T.null title then " " else T.take 60 title))), body ]

-- | Right-align in a column of the given width.
padL :: Int -> Text -> Text
padL w = T.justifyRight w ' '

clampI :: Int -> Int -> Int
clampI n i = max 0 (min (n - 1) i)

-- tree browser ------------------------------------------------------------

data Entry = Loading | Done (Either Text TreeNode)

data TEv = TTick | TLoaded [Text] (Either Text TreeNode) | TDropped [Text]

data TMode = TNav | TFilter | TJump deriving (Eq)

data TSt = TSt
  { tCfg    :: TreeCfg
  , tPath   :: [Text]                  -- directory being listed
  , tCursor :: M.Map [Text] Int        -- remembered cursor per directory
  , tQuery  :: Text
  , tMode   :: TMode
  , tJump   :: Text
  , tCache  :: M.Map [Text] Entry
  , tTick   :: Int
  , tAsk    :: [Text] -> IO ()         -- queue a path for the worker
  , tWanted :: IORef [[Text]]          -- paths the screen still cares about
  }

crumb :: TSt -> [Text] -> Text
crumb _ [] = "."
crumb s p  = T.intercalate (tcSep (tCfg s)) p

branchKids :: TSt -> [Text] -> Maybe [Text]
branchKids s p = case M.lookup p (tCache s) of
  Just (Done (Right (Branch _ _ cs))) -> Just cs
  _                                   -> Nothing

childrenOf :: TSt -> [Text]
childrenOf s = fromMaybe [] (branchKids s (tPath s))

shown :: TSt -> [Text]
shown s = filter ok (childrenOf s)
  where ws   = T.words (T.toLower (tQuery s))
        ok c = all (`T.isInfixOf` T.toLower c) ws

cursorOf :: TSt -> Int
cursorOf s = clampI (length (shown s)) (M.findWithDefault 0 (tPath s) (tCursor s))

selected :: TSt -> Maybe Text
selected s = listToMaybe (drop (cursorOf s) (shown s))

parentOf :: TSt -> Maybe [Text]
parentOf s = if null (tPath s) then Nothing else Just (init (tPath s))

-- | Everything the screen can show: this level, its parent (left column) and
-- the highlighted child (right column).
wantedPaths :: TSt -> [[Text]]
wantedPaths s = nub (catMaybes [Just (tPath s), parentOf s, (\c -> tPath s ++ [c]) <$> selected s])

refresh :: EventM Name TSt ()
refresh = do
  s <- get
  let need    = wantedPaths s
      missing = [ p | p <- need, M.notMember p (tCache s) ]
  liftIO (writeIORef (tWanted s) need)
  liftIO (mapM_ (tAsk s) missing)
  put s { tCache = foldr (\p -> M.insert p Loading) (tCache s) missing }

tHandle :: BrickEvent Name TEv -> EventM Name TSt ()
tHandle (AppEvent TTick)         = modify (\s -> s { tTick = tTick s + 1 })
tHandle (AppEvent (TLoaded p r)) = modify (\s -> s { tCache = M.insert p (Done r) (tCache s) }) >> refresh
tHandle (AppEvent (TDropped p))  = modify (\s -> s { tCache = M.update unload p (tCache s) }) >> refresh
  where unload Loading = Nothing
        unload e       = Just e
tHandle (VtyEvent (V.EvKey k ms)) = do
  s <- get
  case tMode s of
    TNav    -> tNav k ms
    TFilter -> tFilter k ms
    TJump   -> tJumpKey k ms
tHandle _ = pure ()

tNav :: V.Key -> [V.Modifier] -> EventM Name TSt ()
tNav k ms = case (k, ms) of
  (V.KChar 'q', [])        -> halt
  (V.KChar 'c', [V.MCtrl]) -> halt
  (V.KEsc, _)              -> do
    s <- get
    if not (T.null (tQuery s)) then setQuery ""
      else if null (tPath s) then halt else goUp
  (V.KChar 'j', [])        -> move 1
  (V.KDown, _)             -> move 1
  (V.KChar 'k', [])        -> move (-1)
  (V.KUp, _)               -> move (-1)
  (V.KChar 'n', [V.MCtrl]) -> move 1
  (V.KChar 'p', [V.MCtrl]) -> move (-1)
  (V.KChar 'd', [V.MCtrl]) -> move halfPage
  (V.KPageDown, _)         -> move halfPage
  (V.KChar 'u', [V.MCtrl]) -> move (negate halfPage)
  (V.KPageUp, _)           -> move (negate halfPage)
  (V.KChar 'g', [])        -> moveTo 0
  (V.KHome, _)             -> moveTo 0
  (V.KChar 'G', _)         -> moveTo maxBound
  (V.KEnd, _)              -> moveTo maxBound
  (V.KChar 'l', [])        -> enter
  (V.KRight, _)            -> enter
  (V.KEnter, _)            -> enter
  (V.KChar 'h', [])        -> goUp
  (V.KLeft, _)             -> goUp
  (V.KBS, _)               -> goUp
  (V.KChar '/', [])        -> modify (\s -> s { tMode = TFilter })
  (V.KChar '.', [])        -> modify (\s -> s { tMode = TJump, tJump = "" })
  _                        -> pure ()

tFilter :: V.Key -> [V.Modifier] -> EventM Name TSt ()
tFilter k ms = case k of
  V.KEnter -> modify (\s -> s { tMode = TNav })
  V.KEsc   -> modify (\s -> s { tMode = TNav }) >> setQuery ""
  _        -> do
    s <- get
    forM_ (editLine k ms (tQuery s)) setQuery

tJumpKey :: V.Key -> [V.Modifier] -> EventM Name TSt ()
tJumpKey k ms = case k of
  V.KEsc   -> modify (\s -> s { tMode = TNav })
  V.KEnter -> do
    s <- get
    let p = tcSplit (tCfg s) (tJump s)
    put s { tMode = TNav }
    unless (null p) (goTo p)
  _        -> do
    s <- get
    forM_ (editLine k ms (tJump s)) (\t -> modify (\s' -> s' { tJump = t }))

setQuery :: Text -> EventM Name TSt ()
setQuery q = do
  modify (\s -> s { tQuery = q, tCursor = M.insert (tPath s) 0 (tCursor s) })
  refresh

moveTo :: Int -> EventM Name TSt ()
moveTo i = do
  s <- get
  put s { tCursor = M.insert (tPath s) (clampI (length (shown s)) i) (tCursor s) }
  refresh

move :: Int -> EventM Name TSt ()
move d = get >>= \s -> moveTo (cursorOf s + d)

goTo :: [Text] -> EventM Name TSt ()
goTo p = modify (\s -> s { tPath = p, tQuery = "" }) >> refresh

enter :: EventM Name TSt ()
enter = do
  s <- get
  forM_ (selected s) $ \c -> do
    let p = tPath s ++ [c]
    case M.lookup p (tCache s) of
      Just (Done (Right (Leaf _ _))) -> pure ()     -- values have no inside
      _                              -> goTo p

goUp :: EventM Name TSt ()
goUp = do
  s <- get
  unless (null (tPath s)) $ do
    let parent = init (tPath s)
        idx    = fromMaybe 0 (elemIndex (last (tPath s)) (fromMaybe [] (branchKids s parent)))
    put s { tPath = parent, tQuery = "", tCursor = M.insert parent idx (tCursor s) }
    refresh

tDraw :: TSt -> [Widget Name]
tDraw s =
  [ vBox
      [ tTitleBar s
      , hBox [ hLimitPercent 24 (parentPane s), vBorder
             , hLimitPercent 38 (currentPane s), vBorder
             , previewPane s ]
      , tStatusBar s ] ]

tTitleBar :: TSt -> Widget Name
tTitleBar s = padLeftRight 1 $ hBox
  [ hdrW ("❄ " <> tcTitle (tCfg s))
  , dimW ("  " <> tcLabel (tCfg s) <> "  ›  ")
  , txtS (crumb s (tPath s)) ]

tStatusBar :: TSt -> Widget Name
tStatusBar s = padLeftRight 1 $ case tMode s of
  TFilter -> hBox [ withAttr aAccent (txtS "/ "), txtS (tQuery s), cursorMark, dimW "   enter: keep · esc: clear" ]
  TJump   -> hBox [ withAttr aAccent (txtS "go to: "), txtS (tJump s), cursorMark, dimW "   enter: jump · esc: cancel" ]
  TNav    -> hBox
    [ dimW (T.pack (show (min (cursorOf s + 1) n)) <> "/" <> T.pack (show n)
            <> (if T.null (tQuery s) then "" else "  filter: " <> tQuery s) <> "   ")
    , dimW "j/k move · l/⏎ open · h back · / filter · . jump · g/G ends · q quit" ]
  where n = length (shown s)

spinnerW :: Int -> Text -> Widget n
spinnerW tick msg = padLeftRight 1 (withAttr aDim (txtS (spinnerFrame tick <> " " <> msg)))

listBody :: TSt -> [Text] -> ([Text] -> [Text]) -> Maybe Int -> Bool -> Widget Name
listBody s p filt cur focused = case M.lookup p (tCache s) of
  Nothing                     -> padLeftRight 1 (dimW "…")
  Just Loading                -> spinnerW (tTick s) "loading…"
  Just (Done (Left e))        -> padLeftRight 1 (errW e)
  Just (Done (Right (Leaf ty v)))     -> leafW ty v
  Just (Done (Right (Branch _ _ cs))) ->
    let names = filt cs
    in if null names
         then padLeftRight 1 (dimW "(empty)")
         else windowList cur
                [ selRow isCur focused (padLeftRight 1 (entryRow s p isCur c))
                | (i, c) <- zip [0 ..] names, let isCur = cur == Just i ]

leafW :: Text -> Text -> Widget Name
leafW ty v = padLeftRight 1 $ vBox
  [ withAttr aAccent (txtS ty), txtS " ", wrapS (T.take 4000 v) ]

-- | A name plus what we already know about it (if it was visited).
entryRow :: TSt -> [Text] -> Bool -> Text -> Widget Name
entryRow s dir isCur c = hBox [ txtS c, suffix ]
  where
    paint a t = if isCur then txtS t else withAttr a (txtS t)
    suffix = case M.lookup (dir ++ [c]) (tCache s) of
      Just (Done (Right (Branch _ badge _))) -> paint aSet badge
      Just (Done (Right (Leaf _ v)))         -> paint aDim (" = " <> short v)
      Just (Done (Left _))                   -> paint aErr " ✗"
      Just Loading                           -> paint aDim " …"
      Nothing                                -> txtS ""
    short v = let l = T.takeWhile (/= '\n') v
              in if T.length l > 36 then T.take 35 l <> "…" else l

parentPane :: TSt -> Widget Name
parentPane s = case parentOf s of
  Nothing  -> column "" (padLeftRight 1 (dimW "(root)"))
  Just par ->
    let names = fromMaybe [] (branchKids s par)
        cur   = elemIndex (last (tPath s)) names
    in column (crumb s par) (listBody s par id cur False)

currentPane :: TSt -> Widget Name
currentPane s =
  column (crumb s (tPath s) <> count) (listBody s (tPath s) (const (shown s)) (Just (cursorOf s)) True)
  where count = maybe "" (\cs -> "  (" <> T.pack (show (length cs)) <> ")") (branchKids s (tPath s))

previewPane :: TSt -> Widget Name
previewPane s = case selected s of
  Nothing -> column "" (padLeftRight 1 (dimW "(nothing selected)"))
  Just c  -> column c (listBody s (tPath s ++ [c]) id Nothing False)

worker :: ([Text] -> IO (Either Text TreeNode)) -> IORef [[Text]] -> BChan TEv -> Chan [Text] -> IO ()
worker load wanted chan q = forever $ do
  p <- readChan q
  w <- readIORef wanted
  if p `elem` w
    then do
      r <- try (load p)
      writeBChan chan $ TLoaded p $ case r of
        Left (e :: SomeException) -> Left (T.pack (show e))
        Right x                   -> x
    else writeBChan chan (TDropped p)

treeApp :: App TSt TEv Name
treeApp = App
  { appDraw         = tDraw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = tHandle
  , appStartEvent   = refresh
  , appAttrMap      = const browserTheme
  }

-- | Run the tree browser. Returns the path it was left on.
browseTree :: TreeCfg -> IO [Text]
browseTree cfg = do
  chan   <- newBChan 256
  q      <- newChan
  wanted <- newIORef []
  wt     <- forkIO (worker (tcLoad cfg) wanted chan q)
  tk     <- startTicker chan TTick
  let st0 = TSt cfg [] M.empty "" TNav "" M.empty 0 (writeChan q) wanted
  final <- runApp (Just chan) treeApp st0 `finally` (killThread wt >> killThread tk)
  pure (tPath final)

-- graph browser ------------------------------------------------------------

data Side = SRefs | SDeps deriving (Eq)

data GMode = GNav | GFilter deriving (Eq)

data GSt = GSt
  { gCfg   :: GraphCfg
  , gCur   :: Text          -- the item in the middle
  , gHist  :: [Text]        -- where we came from, newest first
  , gSide  :: Side          -- focused list
  , gCR    :: Int           -- cursor in the referrers list
  , gCD    :: Int           -- cursor in the dependencies list
  , gSort  :: Int           -- index into 'sortOpts'
  , gQuery :: Text
  , gMode  :: GMode
  }

-- | Sortable columns, then name.
sortOpts :: GraphCfg -> [(Text, Text -> Text -> Ordering)]
sortOpts cfg =
  [ (colName c, o) | c <- gcColumns cfg, Just o <- [colOrder c] ]
  ++ [("name", \a b -> compare (T.toLower (gcLabel cfg a)) (T.toLower (gcLabel cfg b)))]

currentSort :: GSt -> (Text, Text -> Text -> Ordering)
currentSort s = let os = sortOpts (gCfg s) in os !! (gSort s `mod` length os)

listFor :: GSt -> Side -> [Text]
listFor s side = filter ok (sortBy (snd (currentSort s)) (nub (nbrs (gCur s))))
  where
    cfg  = gCfg s
    nbrs = if side == SRefs then gcRefs cfg else gcDeps cfg
    ws   = T.words (T.toLower (gQuery s))
    ok k = all (`T.isInfixOf` T.toLower (gcLabel cfg k)) ws

cursorFor :: GSt -> Side -> Int
cursorFor s side = clampI (length (listFor s side)) (if side == SRefs then gCR s else gCD s)

setCursor :: Side -> Int -> GSt -> GSt
setCursor SRefs i s = s { gCR = i }
setCursor SDeps i s = s { gCD = i }

highlighted :: GSt -> Text
highlighted s = fromMaybe (gCur s) (listToMaybe (drop (cursorFor s (gSide s)) (listFor s (gSide s))))

resetCursors :: GSt -> GSt
resetCursors s = s { gCR = 0, gCD = 0 }

clearQuery :: GSt -> GSt
clearQuery s = resetCursors s { gQuery = "" }

gOpen :: GSt -> GSt
gOpen s = case drop (cursorFor s (gSide s)) (listFor s (gSide s)) of
  (k : _) -> resetCursors s { gCur = k, gHist = gCur s : gHist s, gSide = SDeps, gQuery = "" }
  []      -> s

-- | Go back one step, with the cursor on the item we just left.
gBack :: GSt -> GSt
gBack s = case gHist s of
  []       -> s
  (p : ps) ->
    let old = gCur s
        s0  = s { gCur = p, gHist = ps, gQuery = "" }
    in case (elemIndex old (listFor s0 SDeps), elemIndex old (listFor s0 SRefs)) of
         (Just i, _) -> (resetCursors s0) { gSide = SDeps, gCD = i }
         (_, Just i) -> (resetCursors s0) { gSide = SRefs, gCR = i }
         _           -> resetCursors s0

gHandle :: BrickEvent Name () -> EventM Name GSt ()
gHandle (VtyEvent (V.EvKey k ms)) = do
  s <- get
  case gMode s of
    GNav    -> gNav s k ms
    GFilter -> gFilter s k ms
gHandle _ = pure ()

gNav :: GSt -> V.Key -> [V.Modifier] -> EventM Name GSt ()
gNav s k ms = case (k, ms) of
  (V.KChar 'q', [])        -> halt
  (V.KChar 'c', [V.MCtrl]) -> halt
  (V.KEsc, _)
    | not (T.null (gQuery s)) -> put (clearQuery s)
    | null (gHist s)          -> halt
    | otherwise               -> put (gBack s)
  (V.KChar '\t', _)        -> put s { gSide = other }
  (V.KBackTab, _)          -> put s { gSide = other }
  (V.KChar 'j', [])        -> mv 1
  (V.KDown, _)             -> mv 1
  (V.KChar 'k', [])        -> mv (-1)
  (V.KUp, _)               -> mv (-1)
  (V.KChar 'n', [V.MCtrl]) -> mv 1
  (V.KChar 'p', [V.MCtrl]) -> mv (-1)
  (V.KChar 'd', [V.MCtrl]) -> mv halfPage
  (V.KPageDown, _)         -> mv halfPage
  (V.KChar 'u', [V.MCtrl]) -> mv (negate halfPage)
  (V.KPageUp, _)           -> mv (negate halfPage)
  (V.KChar 'g', [])        -> mvTo 0
  (V.KHome, _)             -> mvTo 0
  (V.KChar 'G', _)         -> mvTo maxBound
  (V.KEnd, _)              -> mvTo maxBound
  (V.KChar 'l', [])        -> put (gOpen s)
  (V.KRight, _)            -> put (gOpen s)
  (V.KEnter, _)            -> put (gOpen s)
  (V.KChar 'h', [])        -> put (gBack s)
  (V.KLeft, _)             -> put (gBack s)
  (V.KBS, _)               -> put (gBack s)
  (V.KChar '/', [])        -> put s { gMode = GFilter }
  (V.KChar 's', [])        -> put (resetCursors s { gSort = gSort s + 1 })
  _                        -> pure ()
  where
    other = if gSide s == SRefs then SDeps else SRefs
    mvTo i = put (setCursor (gSide s) (clampI (length (listFor s (gSide s))) i) s)
    mv d   = mvTo (cursorFor s (gSide s) + d)

gFilter :: GSt -> V.Key -> [V.Modifier] -> EventM Name GSt ()
gFilter s k ms = case k of
  V.KEnter -> put s { gMode = GNav }
  V.KEsc   -> put (clearQuery s) { gMode = GNav }
  _        -> forM_ (editLine k ms (gQuery s)) $ \q -> put (resetCursors s { gQuery = q })

gDraw :: GSt -> [Widget Name]
gDraw s =
  [ vBox
      [ gTitleBar s
      , hBox [ hLimitPercent 34 (listPane s SRefs "Referrers"), vBorder
             , hLimitPercent 38 (listPane s SDeps "Dependencies"), vBorder
             , detailPane s ]
      , gStatusBar s ] ]

gTitleBar :: GSt -> Widget Name
gTitleBar s = padLeftRight 1 $ hBox
  [ hdrW ("❄ " <> gcTitle cfg)
  , dimW "  "
  , txtS (T.intercalate " › " (map (gcLabel cfg) (reverse (take 4 (gCur s : gHist s)))))
  , dimW ("   " <> gcSummary cfg (gCur s)) ]
  where cfg = gCfg s

gStatusBar :: GSt -> Widget Name
gStatusBar s = padLeftRight 1 $ case gMode s of
  GFilter -> hBox [ withAttr aAccent (txtS "/ "), txtS (gQuery s), cursorMark, dimW "   enter: keep · esc: clear" ]
  GNav    -> dimW $
    "tab pane · j/k move · ⏎/l open · h back · / filter · s sort: " <> fst (currentSort s)
    <> (if T.null (gQuery s) then "" else " · filter: " <> gQuery s) <> " · q quit"

listPane :: GSt -> Side -> Text -> Widget Name
listPane s side title = vBox
  [ padLeftRight 1 (withAttr (if focused then aTitle else aDim)
      (txtS (title <> " (" <> T.pack (show (length xs)) <> ")" <> (if focused then " ◂" else ""))))
  , padLeftRight 1 (dimW header)
  , if null xs then padLeftRight 1 (dimW "(none)")
    else windowList (Just c)
      [ selRow isCur focused (padLeftRight 1 (rowW isCur k)) | (i, k) <- zip [0 ..] xs, let isCur = i == c ]
  ]
  where
    cfg     = gCfg s
    cols    = gcColumns cfg
    xs      = listFor s side
    c       = cursorFor s side
    focused = gSide s == side
    active  = fst (currentSort s)
    header  = T.concat [ padL (colWidth col) (colName col <> (if colName col == active then "▾" else "")) <> " "
                       | col <- cols ] <> "name"
    rowW isCur k = hBox
      ( [ (if isCur || i == (0 :: Int) then id else withAttr aDim)
            (txtS (padL (colWidth col) (colCell col k) <> " "))
        | (i, col) <- zip [0 ..] cols ]
        ++ [ txtS (gcLabel cfg k) ] )

detailPane :: GSt -> Widget Name
detailPane s = padLeftRight 1 (vBox (map render (gcDetails (gCfg s) (highlighted s))))
  where
    render d = case d of
      DHead t    -> hdrW t
      DField l v -> hBox [ dimW (T.justifyLeft 14 ' ' l), txtS v ]
      DText t    -> wrapS t
      DAccent t  -> withAttr aAccent (wrapS t)
      DDim t     -> withAttr aDim (wrapS t)
      DLine t    -> txtS t
      DGap       -> txtS " "

graphApp :: App GSt () Name
graphApp = App
  { appDraw         = gDraw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = gHandle
  , appStartEvent   = pure ()
  , appAttrMap      = const browserTheme
  }

-- | Run the graph browser until the user quits.
browseGraph :: GraphCfg -> IO ()
browseGraph cfg =
  void (runApp Nothing graphApp (GSt cfg (gcStart cfg) [] SDeps 0 0 0 "" GNav))
