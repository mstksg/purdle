
module Data.Trie where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.List.Lazy as List
import Data.List.Lazy (List)

data Trie k v = Trie
    { here  :: Maybe v
    , there :: Map k (Trie k v)
    }

lookup :: forall k v. Ord k => List k -> Trie k v -> Maybe v
lookup ks (Trie tr) = case List.step ks of
    List.Nil        -> tr.here
    List.Cons k ks' -> lookup ks' =<< Map.lookup k tr.there
