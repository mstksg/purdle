
module Data.PositiveInt where

import Prelude
import Data.Maybe

newtype PositiveInt = UnsafePositiveInt Int

derive instance Eq PositiveInt
derive instance Ord PositiveInt

instance Semiring PositiveInt where
    add (UnsafePositiveInt x) (UnsafePositiveInt y) = UnsafePositiveInt (x + y)
    zero = UnsafePositiveInt 0
    mul (UnsafePositiveInt x) (UnsafePositiveInt y) = UnsafePositiveInt (x * y)
    one = UnsafePositiveInt 1

-- | Subtract by 1
decrementPositive :: PositiveInt -> Maybe PositiveInt
decrementPositive (UnsafePositiveInt x)
    | x > 1     = Just (UnsafePositiveInt (x - 1))
    | otherwise = Nothing

unPositive :: PositiveInt -> Int
unPositive (UnsafePositiveInt x) = x
