{-# LANGUAGE OverloadedStrings #-}
-- | nix-tree style data: the closure is loaded once with a single
-- `nix path-info` call; everything else (sorting, referrers, "added size",
-- shortest chain from the root) is computed here, purely.
module Nixon.Nix.Tree
  ( SP (..)
  , Graph (..)
  , loadGraph
  , graphWeight
  , nodeOf
  , chainTo
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Nixon.Core (capture)
import Nixon.Json

-- | One store path.
data SP = SP
  { spPath    :: !Text
  , spNar     :: !Integer     -- own size
  , spClosure :: !Integer     -- size including everything it references
  , spRefs    :: [Text]       -- direct references (self-reference removed)
  }

data Graph = Graph
  { gNodes     :: !(M.Map Text SP)
  , gReferrers :: !(M.Map Text [Text])   -- reverse edges
  , gRoots     :: [Text]
  , gAdded     :: !(M.Map Text Integer)  -- size that only this path keeps alive (dominator subtree)
  , gParent    :: !(M.Map Text Text)     -- breadth-first tree from the roots
  }

-- | Cheap number that forces everything built at load time.
graphWeight :: Graph -> Int
graphWeight g = M.size (gNodes g) + M.size (gAdded g) + M.size (gParent g) + M.size (gReferrers g)

-- | Newer Nix versions may print store paths without the /nix/store/ prefix.
norm :: Text -> Text
norm p | "/" `T.isPrefixOf` p = p
       | otherwise            = "/nix/store/" <> p

-- | `nix path-info -r -S --json` on the given store paths. Handles both the
-- array and the object-keyed JSON layouts.
loadGraph :: [FilePath] -> IO (Maybe Graph)
loadGraph [] = pure Nothing
loadGraph paths = do
  m <- capture (["nix", "path-info", "-r", "-S", "--json"] ++ paths)
  pure $ do
    v <- m >>= parse
    let entries = case v of
          Array _  -> [ (fromMaybe "" (key "path" o >>= strOf), o) | o <- elems v ]
          Object _ -> members v
          _        -> []
        num k o = fromMaybe 0 (key k o >>= intOf)
        mk (p0, o) =
          let p = norm p0
          in SP { spPath    = p
                , spNar     = num "narSize" o
                , spClosure = num "closureSize" o
                , spRefs    = [ r | Just a <- [key "references" o], x <- elems a
                                  , Just r0 <- [strOf x], let r = norm r0, r /= p ] }
        nodes = M.fromList [ (spPath s, s) | e <- entries, let s = mk e ]
        referrers = M.fromListWith (++) [ (r, [spPath s]) | s <- M.elems nodes, r <- spRefs s ]
        rs = case filter (`M.member` nodes) (map (norm . T.pack) paths) of
               [] -> [ p | p <- M.keys nodes, M.notMember p referrers ]
               xs -> xs
    if M.null nodes then Nothing else Just Graph
      { gNodes     = nodes
      , gReferrers = referrers
      , gRoots     = rs
      , gAdded     = addedSizes nodes rs
      , gParent    = bfsParents nodes rs
      }

nodeOf :: Graph -> Text -> Maybe SP
nodeOf g p = M.lookup p (gNodes g)

-- | Shortest chain of paths from a root down to the given path.
chainTo :: Graph -> Text -> [Text]
chainTo g p = reverse (walk p)
  where walk x = x : maybe [] walk (M.lookup x (gParent g))

--  BFS tree ------------------------------------------------------------

bfsParents :: M.Map Text SP -> [Text] -> M.Map Text Text
bfsParents nodes roots = go (S.fromList roots) roots M.empty
  where
    go _ [] par = par
    go seen frontier par =
      let (seen', par', next) = foldl' visit (seen, par, []) frontier
      in go seen' (reverse next) par'
    visit acc p = foldl' (edge p) acc (maybe [] spRefs (M.lookup p nodes))
    edge p (sn, pr, nx) r
      | S.member r sn || M.notMember r nodes = (sn, pr, nx)
      | otherwise = (S.insert r sn, M.insert r p pr, r : nx)

-- added size ------------------------------------------------------------

-- | For every path, the total NAR size of everything that disappears from the
-- closure if the path were dropped: the size of its subtree in the dominator
-- tree (Cooper, Harvey, Kennedy: "A Simple, Fast Dominance Algorithm").
-- A virtual root (index 0) points at the real roots.
addedSizes :: M.Map Text SP -> [Text] -> M.Map Text Integer
addedSizes nodes roots =
  M.fromList [ (p, IM.findWithDefault (spNar sp) i added)
             | (p, sp) <- M.toList nodes, Just i <- [M.lookup p ix] ]
  where
    ix :: M.Map Text Int
    ix = M.fromList (zip (M.keys nodes) [1 ..])

    succs :: IM.IntMap [Int]
    succs = IM.fromList $
      (0, [ i | r <- roots, Just i <- [M.lookup r ix] ])
      : [ (i, [ j | r <- spRefs sp, Just j <- [M.lookup r ix] ])
        | (p, sp) <- M.toList nodes, Just i <- [M.lookup p ix] ]

    preds :: IM.IntMap [Int]
    preds = IM.fromListWith (++) [ (j, [i]) | (i, js) <- IM.toList succs, j <- js ]

    -- reverse post-order from the virtual root
    rpo :: [Int]
    rpo = snd (dfs 0 (IS.empty, []))

    dfs :: Int -> (IS.IntSet, [Int]) -> (IS.IntSet, [Int])
    dfs v (seen, acc)
      | IS.member v seen = (seen, acc)
      | otherwise =
          let (seen', acc') = foldl' (flip dfs) (IS.insert v seen, acc) (IM.findWithDefault [] v succs)
          in (seen', v : acc')

    num :: IM.IntMap Int
    num = IM.fromList (zip rpo [0 ..])

    idom :: IM.IntMap Int
    idom = fixpoint (IM.singleton 0 0)

    fixpoint d =
      let (d', changed) = foldl' step (d, False) (drop 1 rpo)
      in if changed then fixpoint d' else d'

    step (d, ch) b =
      case [ p | p <- IM.findWithDefault [] b preds, IM.member p d ] of
        []        -> (d, ch)
        (p0 : ps) ->
          let new = foldl' (intersect d) p0 ps
          in if IM.lookup b d == Just new then (d, ch) else (IM.insert b new d, True)

    intersect d a b
      | a == b                          = a
      | num IM.! a > num IM.! b         = intersect d (d IM.! a) b
      | otherwise                       = intersect d a (d IM.! b)

    sizes0 :: IM.IntMap Integer
    sizes0 = IM.fromList [ (i, spNar sp) | (p, sp) <- M.toList nodes, Just i <- [M.lookup p ix] ]

    -- children come before their dominator in reverse RPO
    added :: IM.IntMap Integer
    added = foldl' push sizes0 (reverse (drop 1 rpo))

    push m i = case IM.lookup i idom of
      Just d | d /= i -> IM.insertWith (+) d (IM.findWithDefault 0 i m) m
      _               -> m
