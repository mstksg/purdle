
module Purdle.UI.Board where

import Data.Letter
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Halogen as H
import Halogen.Aff.Util as HU
import Halogen.HTML as HH
import Halogen.HTML.Core as HH
import Halogen.HTML.Elements as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Halogen.Util as HU
import Prelude
import Purdle.Types
import Undefined

data GameMode = NormalMode | HardMode | SuperHardMode

data BoardQuery a

type GameSettings =
    { gameMode :: GameMode
    , goalWord :: Word
    }

type BoardState =
    { gameSettings :: GameSettings
    , guessState   :: GuessState
    , typingWord   :: Array Letter
    }

board :: forall m. H.Component BoardQuery GameSettings Void m
board = H.mkComponent
    { initialState: \gameSettings ->
        { guessState: Seq.empty, typingWord: [], gameSettings }
    , render: undefined
    , eval: undefined
    }
