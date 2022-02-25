
module Purdle.UI.Game where

import Control.Alternative
import Control.Monad.State
import Data.Array as Array
import Data.Foldable
import Data.Maybe
import Data.PositiveInt
import Data.PositiveInt
import Data.Traversable
import Effect
import Effect.Class
import Effect.Console as Console
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
import Purdle.UI.Board
import Purdle.WordList
import Type.Proxy
import Undefined

data GameQuery a
data GameOut =
    GOToast String
  | GOGameOver GameEnd

data GameEnd = GameWin Int
             | GameLoss Word

derive instance Eq GameEnd

instance Show GameEnd where
    show = case _ of
      GameWin i -> "(GameWin " <> show i <> ")"
      GameLoss w -> "(GameLoss " <> showWord w <> ")"

data GameAction =
    GABoardOut BoardOut
  | GANewGame
  | GAChangeMode GameMode

data GameProgress =
    GPInitialized
  | GPInProgress PositiveInt        -- ^ guesses left
  | GPGameOver GameEnd

derive instance Eq GameProgress

instance Show GameProgress where
    show = case _ of
      GPInitialized -> "GPInitialized"
      GPInProgress i -> "(GPInProgress " <> show i <> ")"
      GPGameOver e -> "(GPGameOver " <> show e <> ")"

type GameState =
    { goalWord     :: Word
    , gameMode     :: GameMode
    , gameProgress :: GameProgress
    }

toBoardSettings :: GameState -> BoardSettings
toBoardSettings gs =
    { gameMode: gs.gameMode
    , goalWord: gs.goalWord
    , guessLimit: 6
    }

initializeGameState :: GameMode -> Effect GameState
initializeGameState gameMode = do
    goalWord <- randomAnswer
    pure { goalWord, gameMode, gameProgress: GPInitialized }


game :: forall m. MonadEffect m => H.Component GameQuery GameState GameOut m
game = H.mkComponent
    { initialState: identity
    , render: \gst ->
        HH.div [HU.classProp "gameContainer"]
          [ HH.div [HU.classProp "gameContainer-board"]
              [ HH.slot _board unit board (toBoardSettings gst) GABoardOut ]
          , HH.div [HU.classProp "gameContainer-ui"] $ Array.catMaybes
              [ guard (gst.gameProgress /= GPInitialized) $> HH.button
                  [ HU.classProp "ui-key button-new-game"
                  , HP.type_ HP.ButtonButton
                  , HE.onClick \_ -> GANewGame
                  ]
                  [HH.text "New Game"]
              ]
          ]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = case _ of
            GABoardOut bo -> case bo of
              BOToast str -> H.raise (GOToast str)
              BOMadeGuess gu -> do
                gst <- get
                let guessesLeftAfterThis = case gst.gameProgress of
                      GPInitialized  -> toNonNegative 5
                      GPInProgress i -> Just $ decrementPositive i
                      GPGameOver _   -> pure Nothing
                newProgress <- for guessesLeftAfterThis \nni ->
                  if gu == gst.goalWord
                    then do
                      let gw = GameWin $ 6 - unNonNegative nni
                      H.raise $ GOGameOver gw
                      pure $ GPGameOver gw
                    else case nni of
                      Nothing -> do
                        let gl = GameLoss gst.goalWord
                        H.raise $ GOGameOver gl
                        pure $ GPGameOver gl
                      Just i -> do
                        pure $ GPInProgress i
                for_ newProgress \np -> do
                  case np of
                    GPGameOver _ -> void $ H.query _board unit $ FreezeGame unit
                    _            -> pure unit
                  put $ gst { gameProgress = np }
            GANewGame -> do
              oldState <- get
              newWord  <- liftEffect randomAnswer
              gst'     <- modify \gst -> gst
                { goalWord = newWord, gameProgress = GPInitialized }
              void $ H.query _board unit $ NewGame (toBoardSettings gst') unit
              case oldState.gameProgress of
                GPInProgress i -> H.raise (GOGameOver (GameLoss oldState.goalWord))
                _              -> pure unit
            GAChangeMode m -> do
              currProgress  <- gets _.gameProgress
              case currProgress of
                GPInProgress _ -> H.raise $
                  GOToast "Cannot change game mode while game is in progress."
                GPInitialized -> do
                  boardSettings <- toBoardSettings
                    <$> modify (\gst -> gst { gameMode = m })
                  void $ H.query _board unit $ NewGame boardSettings unit
                GPGameOver _ -> modify_ (\gst -> gst { gameMode = m })
        , handleQuery = const (pure Nothing)
        }
    }

-- data BoardOut = BOToast String
--               | BOMadeGuess Word Int

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
