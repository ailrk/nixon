{-# LANGUAGE OverloadedStrings #-}
-- | Nix-specific helpers shared by the browsers. The generic UI toolkit lives
-- in "Gum.Widgets" (gum-brick); no screens and no Nix calls here.
module Nixon.Menu.Browse (menuBrowse) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T


import Control.Monad
import qualified Gum
import Nixon.Core (askDef, pause)

import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Nixon.Nix.Inspect
import Nixon.Nix.Tree
import Nixon.Nix (human, resolveTarget)
import Nixon.Core (menuLoop, opt)

menuBrowse :: IO ()
menuBrowse = menuLoop "Profile"
  [ opt "Browse dependency tree" menuTree
  , opt "Browse config" menuInspect
  ]

-- tree ------------------------------------------------------------
-- pick a target, load its closure once, hand the navigator the graph
-- as plain functions. All UI lives in Gum.Browser.

menuTree :: IO ()
menuTree = do
  mt <- askDef "target" "installable or store path" "/run/current-system"
  forM_ mt $ \t -> do
    ps <- Gum.spin ("Resolving " <> t <> "…") (resolveTarget t)
    mg <- Gum.spinForce "Reading closure…" (maybe 0 graphWeight) (loadGraph ps)
    case mg of
      Just g | (r : _) <- gRoots g -> Gum.browseGraph (graphCfg g r)
      _ -> Gum.bad ("could not read the closure of '" <> t <> "'") >> pause

graphCfg :: Graph -> Text -> Gum.GraphCfg
graphCfg g root = Gum.GraphCfg
  { Gum.gcTitle   = "tree"
  , Gum.gcStart   = root
  , Gum.gcLabel   = storeShort
  , Gum.gcDeps    = \p -> maybe [] spRefs (nodeOf g p)
  , Gum.gcRefs    = \p -> M.findWithDefault [] p (gReferrers g)
  , Gum.gcColumns =
      [ Gum.Column "closure" 8 (sizeCell closure) (Just (biggest closure))
      , Gum.Column "added"   8 (sizeCell added)   (Just (biggest added)) ]
  , Gum.gcSummary = \p -> "closure " <> sizeCell closure p <> " · added " <> sizeCell added p
  , Gum.gcDetails = details g
  }
  where
    closure p = maybe 0 spClosure (nodeOf g p)
    added   p = M.findWithDefault 0 p (gAdded g)
    sizeCell f = human . f
    biggest f = comparing (Down . f)

details :: Graph -> Text -> [Gum.Detail]
details g p = case nodeOf g p of
  Nothing -> [Gum.DDim "(not in the closure)"]
  Just sp ->
    [ Gum.DHead "Details"
    , Gum.DGap
    , Gum.DAccent (storeShort p)
    , Gum.DDim p
    , Gum.DGap
    , Gum.DField "NAR size"     (human (spNar sp))
    , Gum.DField "Closure size" (human (spClosure sp))
    , Gum.DField "Added size"   (human (M.findWithDefault 0 p (gAdded g)))
    , Gum.DDim "  ↳ freed if nothing else needs it"
    , Gum.DField "Dependencies" (T.pack (show (length (spRefs sp))))
    , Gum.DField "Referrers"    (T.pack (show (length (M.findWithDefault [] p (gReferrers g)))))
    , Gum.DGap
    , Gum.DHead "Shortest chain from root"
    ] ++ map Gum.DLine (chainLines (chainTo g p))
  where
    -- long chains stay readable: first 3, a gap, last 8
    chainLines ps
      | length ps <= 12 = zipWith line [0 ..] (map Just ps)
      | otherwise       = zipWith line [0 ..] (map Just (take 3 ps) ++ [Nothing] ++ map Just (drop (length ps - 8) ps))
    line :: Int -> Maybe Text -> Text
    line i mp = indent i <> case mp of
      Nothing -> "…"
      Just q  -> (if i > 0 then "└ " else "") <> storeShort q
    indent i = T.replicate (2 * min i 10) " "

-- inspect ------------------------------------------------------------
-- pick a flake or expression, start the evaluator, hand the navigator a
-- loader. All UI lives in Gum.Browser.

-- | Our evaluator's node as the navigator's node.
toTree :: Node -> Gum.TreeNode
toTree n = case ndKind n of
  KLeaf -> Gum.Leaf   (ndType n) (fromMaybe "" (ndValue n))
  KSet  -> Gum.Branch (ndType n) "/" (ndChildren n)
  KDrv  -> Gum.Branch (ndType n) " ‹drv›" (ndChildren n)
  KList -> Gum.Branch (ndType n) (" [" <> T.pack (show (length (ndChildren n))) <> "]") (ndChildren n)

menuInspect :: IO ()
menuInspect = do
  mref <- askDef "flake" "path, flake ref, or  expr:<nix expression>" "."
  forM_ mref $ \ref -> do
    root <- rootExpr ref
    r <- withRepl root $ \repl ->
      Gum.browseTree
        (Gum.treeCfg "inspect" (T.take 40 ref) (fmap (fmap toTree) . replInspect repl))
          { Gum.tcSplit = splitAttrPath }
    case r of
      Left e  -> Gum.bad e >> pause
      Right p -> unless (null p) (Gum.dim ("last path: " <> breadcrumb p))

-- core ------------------------------------------------------------

-- | "a.b.c" for a path, "." for the root.
breadcrumb :: [Text] -> Text
breadcrumb [] = "."
breadcrumb p  = T.intercalate "." p

-- | Drop the "/nix/store/<hash>-" prefix; anything that does not look like a
-- store path is returned unchanged.
storeShort :: Text -> Text
storeShort p
  | T.length b > 33 && T.index b 32 == '-' = T.drop 33 b
  | otherwise                              = p
  where b = fromMaybe p (T.stripPrefix "/nix/store/" p)
