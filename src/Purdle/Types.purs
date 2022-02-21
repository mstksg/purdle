
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

