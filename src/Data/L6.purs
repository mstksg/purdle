
module Data.L6 where

import Prelude
import Data.Maybe
import Data.Foldable
import Data.List.Lazy as List

data L6 a =
    L6_0
  | L6_1 a
  | L6_2 a a
  | L6_3 a a a
  | L6_4 a a a a
  | L6_5 a a a a a
  | L6_6 a a a a a a

derive instance Functor L6

l6ToList :: forall a. L6 a -> List.List a
l6ToList = case _ of
    L6_0 -> List.nil
    L6_1 x1 -> x1 List.: List.nil
    L6_2 x1 x2 -> x1 List.: x2 List.: List.nil
    L6_3 x1 x2 x3 -> x1 List.: x2 List.: x3 List.: List.nil
    L6_4 x1 x2 x3 x4 -> x1 List.: x2 List.: x3 List.: x4 List.: List.nil
    L6_5 x1 x2 x3 x4 x5 -> x1 List.: x2 List.: x3 List.: x4 List.: x5 List.: List.nil
    L6_6 x1 x2 x3 x4 x5 x6 -> x1 List.: x2 List.: x3 List.: x4 List.: x5 List.: x5 List.: List.nil

instance foldableL6 :: Foldable L6 where
    foldMap f = foldMap f <<< l6ToList
    foldr f z = foldr f z <<< l6ToList
    foldl f z = foldl f z <<< l6ToList

consL6 :: forall a. a -> L6 a -> Maybe (L6 a)
consL6 y = case _ of
    L6_0 -> Just $ L6_1 y
    L6_1 x1 -> Just $ L6_2 y x1
    L6_2 x1 x2 -> Just $ L6_3 y x1 x2
    L6_3 x1 x2 x3 -> Just $ L6_4 y x1 x2 x3
    L6_4 x1 x2 x3 x4 -> Just $ L6_5 y x1 x2 x3 x4
    L6_5 x1 x2 x3 x4 x5 -> Just $ L6_6 y x1 x2 x3 x4 x5
    L6_6 _  _  _  _  _  _  -> Nothing

lastL6 :: forall a. L6 a -> Maybe a
lastL6 = case _ of
    L6_0 -> Nothing
    L6_1 x1 -> Just x1
    L6_2 _  x2 -> Just x2
    L6_3 _  _  x3 -> Just x3
    L6_4 _  _  _  x4 -> Just x4
    L6_5 _  _  _  _  x5 -> Just x5
    L6_6 _  _  _  _  _  x6 -> Just x6

