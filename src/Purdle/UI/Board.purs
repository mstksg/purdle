
module Purdle.UI.Board where

import Control.Apply
import Control.Monad.State
import Data.Array as Array
import Data.Letter
import Data.Either
import Data.Lazy
import Data.Foldable
import Data.List.Lazy as List
import Data.Maybe
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Trie (Trie)
import Data.Trie as Trie
import Data.V5
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
import Purdle.Evaluate
import Purdle.Summary
import Purdle.Types
import Purdle.UI.WordPicker
import Type.Proxy
import Undefined

data GameMode = NormalMode | HardMode | SuperHardMode

data BoardQuery a

data GameEnd = GameWin Int
             | GameLoss Word

data BoardOut = BOToast String
              | BOEndGame GameEnd

type GameSettings =
    { gameMode   :: GameMode
    , goalWord   :: Word
    , guessLimit :: Int
    }

type BoardState =
    { gameSettings :: GameSettings
    , guessState   :: GuessState
    , typingWord   :: Seq Letter
    , lastWordBad  :: Boolean
    , gameOver     :: Boolean
    }

type Dictionary = Trie Letter String

board :: forall m. Dictionary -> H.Component BoardQuery GameSettings BoardOut m
board dict = H.mkComponent
    { initialState: \gameSettings ->
        { guessState: Seq.empty
        , typingWord: Seq.empty
        , lastWordBad: false
        , gameOver: false
        , gameSettings
        }
    , render: \boardState ->
        HH.div [HU.classProp "gameBoard"]
          [ renderBoardGrid boardState
          , HH.div [HU.classProp "gameBoard-keyboard"]
              [ HH.slot _wordPicker unit (wordPicker 5) boardState.typingWord identity
              ]
          ]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleActionBoard dict
        }
    }

renderBoardGrid :: forall a b. BoardState -> HH.HTML a b
renderBoardGrid boardState = HH.ul [HU.classProp "gameBoard-grid"] $
  Array.fromFoldable $
    flip map (layoutGrid boardState) \{isEntry, boxes} ->
      HH.li [HU.classProp $ "gameBoard-grid-row "
                       <> isWrongClass boardState.lastWordBad
            ]
        [ HH.ul [HU.classProp "gameBoard-grid-row-list"] $
            Array.fromFoldable $
              flip map boxes case _ of
                EvalLetter le l ->
                  HH.li [HU.classProp $ "gameBoard-grid-item " <> evalClass le]
                    [HH.text (show l)]
                InputLetter l ->
                  HH.li [HU.classProp "gameBoard-grid-item entry-input"]
                    [HH.text (show l)]
                BlankBox ->
                  HH.li [HU.classProp "gameBoard-grid-item blank-box"]
                    []
        ]

handleActionBoard
    :: forall m.
       Dictionary
    -> WordPickerOut
    -> H.HalogenM BoardState WordPickerOut (wordPicker :: H.Slot WordPickerQuery WordPickerOut Unit) BoardOut m Unit
handleActionBoard dict act = do
  gameIsOver <- gets \bs -> bs.gameOver
  unless gameIsOver case act of
    WPQInProgress w -> modify_ \bs -> bs { typingWord = w }
    WPQSubmit w -> do
      boardState <- modify \bs -> bs { lastWordBad = false }
      let summary = defer \_ -> colorSummary
            { goalWord: boardState.gameSettings.goalWord
            , guessState: boardState.guessState
            }
          validatedGuess = do
            wVec <- case v5FromListExact (Array.fromFoldable w) of
              Nothing -> Left ["Not enough letters"]
              Just w  -> Right w
            let goodHardMode = flip validHardMode wVec <$> summary
                goodSuperHardMode = flip validSuperHardMode wVec <$> summary
            case List.fromFoldable w `Trie.lookup` dict of
              Nothing -> Left ["Not in word list"]
              Just _  -> Right unit
            case boardState.gameSettings.gameMode of
              NormalMode -> Right unit
              HardMode   -> case showHardModeErrors (force goodHardMode) of
                Nothing -> Right unit
                Just es -> Left $ Array.fromFoldable es
              SuperHardMode -> case showSuperHardModeErrors (force goodSuperHardMode) of
                Nothing -> Right unit
                Just es -> Left $ Array.fromFoldable es
            pure wVec
      case validatedGuess of
        Left  es -> do
           traverse_ (H.raise <<< BOToast) es
           modify_ \bs -> bs { lastWordBad = true }
        Right gs -> do
          modify_ \bs -> bs { guessState = bs.guessState `Seq.snoc` gs }
          if (gs == boardState.gameSettings.goalWord)
            then do
              H.raise $ BOEndGame (GameWin (Seq.length boardState.guessState + 1))
              modify_ \bs -> bs { gameOver = true }
            else
              when (Seq.length boardState.guessState + 1 >= boardState.gameSettings.guessLimit) $ do
                H.raise $ BOEndGame (GameLoss (boardState.gameSettings.goalWord))
                modify_ \bs -> bs { gameOver = true }

evalClass :: LetterEval -> String
evalClass = case _ of
    NotInWord -> "eval-not-in-word"
    WrongPos  -> "eval-wrong-pos"
    RightPos  -> "eval-right-pos"

isWrongClass :: Boolean -> String
isWrongClass isWrong
    | isWrong   = "invalid-guess"
    | otherwise = ""

data GridBox = EvalLetter LetterEval Letter
             | InputLetter Letter
             | BlankBox

layoutGrid :: BoardState -> Seq { isEntry :: Boolean, boxes :: V5 GridBox }
layoutGrid boardState = Seq.fromFoldable $
                        List.take boardState.gameSettings.guessLimit $
        evalGrid
     <> (pickerGrid List.: List.repeat { isEntry: false, boxes: pure BlankBox })
  where
    evalGrid = flip map (List.fromFoldable boardState.guessState) \wd ->
      { isEntry: false
      , boxes: lift2 EvalLetter
            (evalGuess boardState.gameSettings.goalWord wd)
            wd
      }
    pickerGrid =
      { isEntry: true
      , boxes: map (maybe BlankBox InputLetter)
          (v5FromList (List.fromFoldable boardState.typingWord)).res
      }

_wordPicker :: Proxy "wordPicker"
_wordPicker = Proxy
