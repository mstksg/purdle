
module Purdle.Types where

import Data.Letter
import Data.Sequence (Seq)
import Data.Trie (Trie)
import Data.V5
import Prelude
import Data.Foldable

type Word = V5 Letter

showWord :: Word -> String
showWord = foldMap show

type GuessState = Seq Word

type GameInfo =
    { goalWord :: Word
    , guessState :: GuessState
    }

type Dictionary = Trie Letter Word

