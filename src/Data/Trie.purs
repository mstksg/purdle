
module Data.Trie where

import Data.Bifunctor
import Data.Foldable
import Data.FoldableWithIndex
import Data.Lazy
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.Maybe.First
import Data.Tuple
import Prelude

data Trie k v = Trie
    { here  :: Maybe v
    , there :: Map k (Lazy (Trie k v))
    }

lookup :: forall k v. Ord k => List k -> Trie k v -> Maybe v
lookup ks (Trie tr) = case List.step ks of
    List.Nil        -> tr.here
    List.Cons k ks' -> lookup ks' <<< force =<< Map.lookup k tr.there

empty :: forall k v. Trie k v
empty = Trie { here: Nothing, there: Map.empty }

fromMapFoldable :: forall f k v. Foldable f => Ord k => Map (f k) v -> Trie k v
fromMapFoldable = fromMap
              <<< (Map.fromFoldable :: List (Tuple (List k) v) -> Map (List k) v)
              <<< map (lmap List.fromFoldable)
              <<< Map.toUnfoldableUnordered

fromMap :: forall k v. Ord k => Map (List k) v -> Trie k v
fromMap mp = Trie {here, there}
  where
    -- here = Nothing
    Tuple (First here) (Map.SemigroupMap thereList) = flip foldMapWithIndex mp \ks v ->
      case List.step ks of
        List.Nil -> Tuple (First (Just v)) mempty
        List.Cons k ks' -> Tuple mempty $ Map.SemigroupMap $
          Map.singleton k $
            List.singleton $ Tuple ks' v
    there = map (\ls -> defer \_ -> fromMap (Map.fromFoldable ls)) thereList
