
module Purdle.UI.Game where

import Control.Alternative
import Control.Monad.State
import Data.Array as Array
import Data.Foldable
import Data.Maybe
import Purdle.Evaluate
import Data.PositiveInt
import Data.List.Lazy as List
import Data.PositiveInt
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable
import Data.Trie as Trie
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
import Purdle.Solver.Filter
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

gpInProgress :: GameProgress -> Maybe PositiveInt
gpInProgress = case _ of
    GPInitialized  -> Nothing
    GPInProgress i -> Just i
    GPGameOver _   -> Nothing

instance Show GameProgress where
    show = case _ of
      GPInitialized -> "GPInitialized"
      GPInProgress i -> "(GPInProgress " <> show i <> ")"
      GPGameOver e -> "(GPGameOver " <> show e <> ")"

type GameState =
    { goalWord     :: Word
    , gameMode     :: GameMode
    , gameProgress :: GameProgress
    , gameClues    :: Set Clue
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
    pure { goalWord, gameMode, gameProgress: GPInitialized, gameClues: Set.empty }

showWithCount :: Int -> String -> String
showWithCount i wd
    | i == 1    = show i <> " " <> wd
    | otherwise = show i <> " " <> wd <> "s"

wordsLeft :: Set Clue -> Int
wordsLeft ls = maybe 0 Set.size $
    List.fromFoldable ls `Trie.lookup` allowedWordsClueTrie

game :: forall m. MonadEffect m => H.Component GameQuery GameState GameOut m
game = H.mkComponent
    { initialState: identity
    , render: \gst ->
        HH.div [HU.classProp "gameContainer"]
          [ HH.div [HU.classProp "gameContainer-board"]
              [ HH.slot _board unit board (toBoardSettings gst) GABoardOut ]
          , HH.div [HU.classProp "gameContainer-ui"] $ Array.catMaybes
              [ Just $ HH.div
                    [HU.classProp "ui-text-leftover"]
                    [ HH.span []
                        [ HH.text $
                            showWithCount (wordsLeft gst.gameClues) "word"
                         <> " remaining"
                        ]
                    ]
              , Just $ HH.select
                  [ HU.classProp "ui-select gameMode-picker"
                  , HP.disabled (isJust (gpInProgress gst.gameProgress))
                  , HE.onSelectedIndexChange \i -> GAChangeMode $
                      fromMaybe gst.gameMode (gameModes `Array.index` i)
                  ]
                  (flip map gameModes \gm ->
                      HH.option
                        [HP.selected (gm == gst.gameMode)]
                        [HH.text (showGameMode gm)]
                  )
              , guard (gst.gameProgress /= GPInitialized) $> HH.button
                  [ HU.classProp "ui-key button-new-game"
                  , HP.type_ HP.ButtonButton
                  , HE.onClick \_ -> GANewGame
                  ]
                  [HH.text "New Game"]
              ]
          ]
    -- scald: peats
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
                progClues <- for guessesLeftAfterThis \nni -> do
                  let clues = cluesForWord gu (evalGuess gst.goalWord gu)
                  newProgress <- if gu == gst.goalWord
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
                  pure { clues, newProgress }
                for_ progClues \{clues, newProgress} -> do
                  case newProgress of
                    GPGameOver _ -> void $ H.query _board unit $ FreezeGame unit
                    _            -> pure unit
                  put $ gst
                    { gameProgress = newProgress
                    , gameClues    = gst.gameClues <> Set.fromFoldable clues
                    }
            GANewGame -> do
              oldState <- get
              newWord  <- liftEffect randomAnswer
              liftEffect $ Console.log (showWord newWord)
              gst'     <- modify \gst -> gst
                { goalWord = newWord
                , gameProgress = GPInitialized
                , gameClues = Set.empty :: Set Clue
                }
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

_board :: Proxy "board"
_board = Proxy
