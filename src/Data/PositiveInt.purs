
module Data.PositiveInt where

import Data.Either
import Data.Maybe
import Data.Ord
import Prelude

newtype PositiveInt = UnsafePositiveInt Int

derive instance Eq PositiveInt
derive instance Ord PositiveInt

instance Show PositiveInt where
    show (UnsafePositiveInt i) = show i

-- | Addition
instance Semigroup PositiveInt where
    append (UnsafePositiveInt x) (UnsafePositiveInt y) = UnsafePositiveInt (x + y)

positiveOne :: PositiveInt
positiveOne = UnsafePositiveInt 1

positiveTwo :: PositiveInt
positiveTwo = UnsafePositiveInt 2

positiveThree :: PositiveInt
positiveThree = UnsafePositiveInt 3

positiveFour :: PositiveInt
positiveFour = UnsafePositiveInt 4

positiveFive :: PositiveInt
positiveFive = UnsafePositiveInt 5


-- | Subtract by 1
decrementPositive :: PositiveInt -> Maybe PositiveInt
decrementPositive (UnsafePositiveInt x)
    | x > 1     = Just (UnsafePositiveInt (x - 1))
    | otherwise = Nothing

unPositive :: PositiveInt -> Int
unPositive (UnsafePositiveInt x) = x

toPositive :: Int -> Maybe PositiveInt
toPositive x
    | x > 0     = Just (UnsafePositiveInt x)
    | otherwise = Nothing

type NonNegativeInt = Maybe PositiveInt

toNonNegative :: Int -> Maybe NonNegativeInt
toNonNegative i = case classifyInt i of
    Nothing        -> Just Nothing
    Just (Left _ ) -> Nothing
    Just (Right i) -> Just (Just i)
    

decrementNonNegative :: NonNegativeInt -> Maybe NonNegativeInt
decrementNonNegative = map decrementPositive

unNonNegative :: NonNegativeInt -> Int
unNonNegative = maybe 0 unPositive

-- | 0, or negative, or positive
classifyInt :: Int -> Maybe (Either PositiveInt PositiveInt)
classifyInt i = case compare i 0 of
    LT -> Just $ Left (UnsafePositiveInt (abs i))
    EQ -> Nothing
    GT -> Just $ Right (UnsafePositiveInt i)
    
