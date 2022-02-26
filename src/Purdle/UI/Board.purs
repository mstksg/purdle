
module Purdle.UI.Board where

import Control.Apply
import Control.Monad.State
import Data.Array as Array
import Data.Either
import Data.Foldable
import Data.Lazy
import Data.Letter
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Trie (Trie)
import Data.Trie as Trie
import Data.V5
import Effect.Class
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
import Purdle.UI.Keyboard
import Purdle.UI.WordPicker
import Purdle.Validate
import Purdle.WordList
import Type.Proxy
import Undefined

data GameMode = NormalMode | HardMode | SuperHardMode

showGameMode :: GameMode -> String
showGameMode = case _ of
    NormalMode    -> "Normal Mode"
    HardMode      -> "Hard Mode"
    SuperHardMode -> "Super Hard Mode"

gameModes :: Array GameMode
gameModes = [NormalMode, HardMode, SuperHardMode]

derive instance Eq GameMode

data BoardQuery a = NewGame BoardSettings a
                  | FreezeGame a

data BoardOut = BOToast String
              | BOMadeGuess Word

type BoardSettings =
    { gameMode   :: GameMode
    , goalWord   :: Word
    , guessLimit :: Int
    }

type BoardState =
    { boardSettings :: BoardSettings
    , guessState    :: GuessState
    , typingWord    :: Seq Letter
    , lastWordBad   :: Boolean
    , gameActive    :: Boolean
    }

newBoardState :: BoardSettings -> BoardState
newBoardState boardSettings =
    { guessState: Seq.empty
    , typingWord: Seq.empty
    , lastWordBad: false
    , gameActive: true
    , boardSettings
    }

board :: forall m. MonadEffect m => H.Component BoardQuery BoardSettings BoardOut m
board = H.mkComponent
    { initialState: newBoardState
    , render: \boardState ->
        HH.div [HU.classProp "gameBoard"]
          [ renderBoardGrid boardState
          , HH.div [HU.classProp "gameBoard-keyboard"]
              [ HH.slot _wordPicker unit (wordPicker 5) boardState.typingWord identity
              ]
          ]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleWordPickerOut
        , handleQuery = case _ of
            NewGame gs r -> Just r <$ do
               void $ H.query _wordPicker unit (WPQReset unit)
               void $ H.query _wordPicker unit (WPQSetColors Map.empty unit)
               put (newBoardState gs)
            FreezeGame r -> Just r <$ do
               void $ H.query _wordPicker unit (WPQReset unit)
               modify_ (\bs -> bs { gameActive = false })
        }
    }

renderBoardGrid :: forall a b. BoardState -> HH.HTML a b
renderBoardGrid boardState = HH.ul [HU.classProp "gameBoard-grid"] $
  Array.fromFoldable $
    flip map (layoutGrid boardState) \{isEntry, boxes} ->
      HH.li [HU.classProp $ "gameBoard-grid-row "
                       <> if isEntry then isWrongClass boardState.lastWordBad else ""
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

handleWordPickerOut
    :: forall m.
       WordPickerOut
    -> H.HalogenM BoardState _ (wordPicker :: H.Slot WordPickerQuery WordPickerOut Unit) BoardOut m Unit
handleWordPickerOut act = do
  isActive <- gets \bs -> bs.gameActive
  when isActive case act of
    WPQInProgress w -> modify_ \bs -> bs { typingWord = w, lastWordBad = false }
    WPQSubmit w -> do
      boardState <- get
      let summary = defer \_ -> colorSummary
            { goalWord: boardState.boardSettings.goalWord
            , guessState: boardState.guessState
            }
          validatedGuess = do
            wVec <- case v5FromListExact (Array.fromFoldable w) of
              Nothing -> Left ["Not enough letters"]
              Just w  -> Right w
            let goodHardMode = flip validHardMode wVec <$> summary
                goodSuperHardMode = flip validSuperHardMode wVec <$> summary
            case List.fromFoldable w `Trie.lookup` defaultDictionary of
              Nothing -> Left ["Not in word list: " <> showWord wVec]
              Just _  -> Right unit
            case boardState.boardSettings.gameMode of
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
          -- let newLength = Seq.length boardState.guessState + 1
          H.raise (BOMadeGuess gs)
          bs <- modify \bs -> bs
            { guessState = bs.guessState `Seq.snoc` gs
            , lastWordBad = false
            }
          _ <- H.query _wordPicker unit $ WPQReset unit
          let colorMap = letterColors
                { goalWord: bs.boardSettings.goalWord
                , guessState: bs.guessState
                }
          _ <- H.query _wordPicker unit $ WPQSetColors (map leToColor colorMap) unit
          pure unit

evalClass :: LetterEval -> String
evalClass = case _ of
    NotInWord -> "eval-not-in-word"
    WrongPos  -> "eval-wrong-pos"
    RightPos  -> "eval-right-pos"

isWrongClass :: Boolean -> String
isWrongClass isWrong
    | isWrong   = "invalid-guess"
    | otherwise = ""

leToColor :: LetterEval -> Color
leToColor = case _ of
    NotInWord -> Black
    WrongPos  -> Yellow
    RightPos  -> Green

data GridBox = EvalLetter LetterEval Letter
             | InputLetter Letter
             | BlankBox

layoutGrid :: BoardState -> Seq { isEntry :: Boolean, boxes :: V5 GridBox }
layoutGrid boardState = Seq.fromFoldable $
                        List.take boardState.boardSettings.guessLimit $
        evalGrid
     <> (pickerGrid List.: List.repeat { isEntry: false, boxes: pure BlankBox })
  where
    evalGrid = flip map (List.fromFoldable boardState.guessState) \wd ->
      { isEntry: false
      , boxes: lift2 EvalLetter
            (evalGuess boardState.boardSettings.goalWord wd)
            wd
      }
    pickerGrid =
      { isEntry: true
      , boxes: map (maybe BlankBox InputLetter)
          (v5FromList (List.fromFoldable boardState.typingWord)).res
      }

_wordPicker :: Proxy "wordPicker"
_wordPicker = Proxy
