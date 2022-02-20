
module Purdle.Types where

import Data.Letter
import Data.Sequence (Seq)
import Data.V5
import Prelude

type Word = V5 Letter

type GuessState = Seq Word

type GameInfo =
    { goalWord :: Word
    , guessState :: GuessState
    }

data Color = Black | Yellow | Green

derive instance Eq Color
derive instance Ord Color

instance Show Color where
    show = case _ of
      Black  -> "Black"
      Yellow -> "Yellow"
      Green  -> "Green"

instance Semigroup Color where
    append = max

