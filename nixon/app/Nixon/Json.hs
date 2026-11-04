{-# LANGUAGE OverloadedStrings #-}
-- | Tiny aeson helpers; this is what replaces every @jq@ call in the bash script.
module Nixon.Json
  ( Value (..)
  , parse, key, at, strOf, strsOf, intField, intOf, members, elems, pretty
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

parse :: Text -> Maybe Value
parse = A.decodeStrict . TE.encodeUtf8

key :: Text -> Value -> Maybe Value
key k (Object o) = KM.lookup (K.fromText k) o
key _ _          = Nothing

-- | Follow a path of object keys.
at :: [Text] -> Value -> Maybe Value
at ks v = foldM (flip key) v ks

strOf :: Value -> Maybe Text
strOf (String s) = Just s
strOf _          = Nothing

-- | All strings in a JSON array (empty if absent or not an array).
strsOf :: Maybe Value -> [Text]
strsOf mv = [ s | Just a <- [mv], x <- elems a, Just s <- [strOf x] ]

intOf :: Value -> Maybe Integer
intOf (Number n) = Just (truncate n)
intOf _          = Nothing

intField :: Text -> Value -> Maybe Integer
intField k o = key k o >>= intOf

-- | Object members, or array elements keyed by index (like jq's to_entries).
members :: Value -> [(Text, Value)]
members (Object o) = [ (K.toText k, v) | (k, v) <- KM.toList o ]
members (Array xs) = zip (map (T.pack . show) [0 :: Int ..]) (toList xs)
members _          = []

elems :: Value -> [Value]
elems (Array xs) = toList xs
elems _          = []

pretty :: Value -> Text
pretty = TE.decodeUtf8 . BL.toStrict . AP.encodePretty
