{-# LANGUAGE OverloadedStrings #-}
-- | nix-tree style browsing: the closure is loaded once with a single
-- `nix path-info` call, after which all navigation is pure Map lookups.
module Nixon.Browse.Tree
  ( SP (..)
  , Graph (..)
  , View (..)
  , loadGraph
  , nodeOf
  , neighbours
  , toggle
  , whyCmd
  ) where

import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord (Down (..))
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
  }

data View = Deps | Referrers deriving (Eq)

toggle :: View -> View
toggle Deps      = Referrers
toggle Referrers = Deps

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
    if M.null nodes then Nothing else Just Graph
      { gNodes     = nodes
      , gReferrers = M.fromListWith (++) [ (r, [spPath s]) | s <- M.elems nodes, r <- spRefs s ]
      , gRoots     = map (norm . T.pack) paths
      }

nodeOf :: Graph -> Text -> Maybe SP
nodeOf g p = M.lookup p (gNodes g)

-- | Dependencies or referrers of a path, biggest closure first.
neighbours :: Graph -> View -> Text -> [SP]
neighbours g view p =
  sortOn (Down . spClosure) (mapMaybe (nodeOf g) names)
  where
    names = case view of
      Deps      -> maybe [] spRefs (nodeOf g p)
      Referrers -> M.findWithDefault [] p (gReferrers g)

-- | Why does @root@ depend on @target@?
whyCmd :: Text -> Text -> [String]
whyCmd root target = ["nix", "why-depends", "--all", T.unpack root, T.unpack target]
