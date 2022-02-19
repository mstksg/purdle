
module Purdle.Types where

import Prelude
import Data.V5
import Data.L6

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M
            | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

derive instance Eq Letter
derive instance Ord Letter

type Word = V5 Letter

type GuessState = L6 Word

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

