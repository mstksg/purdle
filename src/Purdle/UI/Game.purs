
module Purdle.UI.Game where

import Prelude
import Data.Maybe
import Type.Proxy
import Halogen as H
import Purdle.WordList
import Effect.Class
import Halogen.Aff.Util as HU
import Halogen.HTML as HH
import Effect
import Halogen.HTML.Core as HH
import Halogen.HTML.Elements as HH
import Purdle.UI.Board
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Halogen.Util as HU
import Undefined

data GameQuery a
data GameOut

data GameAction =
    GABoardOut BoardOut

-- type GameState = {}

-- type GameSettings =
--     { gameMode   :: GameMode
--     , goalWord   :: Word
--     , guessLimit :: Int
--     }

initializeGameSettings :: GameMode -> Effect GameSettings
initializeGameSettings gameMode = do
    goalWord <- randomAnswer
    pure { gameMode, goalWord, guessLimit: 6 }


game :: forall m. MonadEffect m => H.Component GameQuery GameSettings GameOut m
game = H.mkComponent
    { initialState: identity
    , render: \gset ->
        HH.div [HU.classProp "gameContainer"]
          [ HH.div [HU.classProp "gameContainer-board"]
              [ HH.slot _board unit board gset GABoardOut ]
          ]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = case _ of
            GABoardOut bo -> undefined
        , handleQuery = const (pure Nothing)
        }
    }

--     { initialState: newBoardState
--     , render: \boardState ->
--         HH.div [HU.classProp "gameBoard"]
--           [ renderBoardGrid boardState
--           , HH.div [HU.classProp "gameBoard-keyboard"]
--               [ HH.slot _wordPicker unit (wordPicker 5) boardState.typingWord identity
--               ]
--           ]
--     , eval: H.mkEval $ H.defaultEval
--         { handleAction = handleWordPickerOut
--         , handleQuery = case _ of
--             NewGame gs r -> Just r <$ put (newBoardState gs)
--         }
--     }

_board :: Proxy "board"
_board = Proxy
