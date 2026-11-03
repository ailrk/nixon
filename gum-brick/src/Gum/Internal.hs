{-# LANGUAGE OverloadedStrings #-}
module Gum.Internal
  ( Res (..)
  , theme, aSel, aDim, aAccent
  , sanitize, txtS
  ) where

import Brick
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V

-- | Resource names (only the pager needs a real one).
data Res = NView deriving (Eq, Ord, Show)

aSel, aDim, aAccent :: AttrName
aSel    = attrName "gum-sel"
aDim    = attrName "gum-dim"
aAccent = attrName "gum-accent"

theme :: AttrMap
theme = attrMap V.defAttr
  [ (aSel,    V.withStyle (fg V.cyan) V.bold)
  , (aDim,    V.withStyle (fg V.white) V.dim)
  , (aAccent, fg V.cyan)
  ]

-- | Make arbitrary text safe to hand to Brick (tabs/CRs would corrupt layout).
sanitize :: Text -> Text
sanitize = T.replace "\t" "    " . T.replace "\r" ""

-- | 'txt' that never renders an empty (zero-width) widget.
txtS :: Text -> Widget n
txtS t = txt (if T.null t then " " else sanitize t)
