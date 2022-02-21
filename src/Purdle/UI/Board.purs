
module Purdle.UI.Board where

import Data.Letter
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Control.Apply
import Data.V5
import Data.Maybe
import Data.Array as Array
import Data.List.Lazy as List
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
import Purdle.Types
import Purdle.UI.WordPicker
import Type.Proxy
import Undefined

data GameMode = NormalMode | HardMode | SuperHardMode

data BoardQuery a

data BoardOut = BOToast String

type GameSettings =
    { gameMode   :: GameMode
    , goalWord   :: Word
    , guessLimit :: Int
    }

type BoardState =
    { gameSettings :: GameSettings
    , guessState   :: GuessState
    , typingWord   :: Array Letter
    , lastWordBad  :: Boolean
    }

board :: forall m. H.Component BoardQuery GameSettings BoardOut m
board = H.mkComponent
    { initialState: \gameSettings ->
        { guessState: Seq.empty, typingWord: [], lastWordBad: false, gameSettings }
    , render: \boardState ->
        HH.div [HU.classProp "gameBoard"]
          [ renderBoardGrid boardState
          , HH.div [HU.classProp "gameBoard-keyboard"]
              [ HH.slot _wordPicker unit (wordPicker 5) boardState.typingWord identity
              ]
          ]
    , eval: undefined
    }

renderBoardGrid :: forall a b. BoardState -> HH.HTML a b
renderBoardGrid boardState = HH.ul [HU.classProp "gameBoard-grid"] $
  Array.fromFoldable $
    flip map (layoutGrid boardState) \boxRow ->
      HH.li [HU.classProp "gameBoard-grid-row"] $
        [ HH.ul [HU.classProp "gameBoard-grid-row-list"] $
            Array.fromFoldable $
              flip map boxRow case _ of
                EvalLetter le l ->
                  HH.li [HU.classProp $ "gameBoard-grid-item " <> evalClass le]
                    [HH.text (show l)]
                InputLetter l ->
                  HH.li [HU.classProp $
                            "gameBoard-grid-item "
                            <> isWrongClass boardState.lastWordBad
                        ]
                    [HH.text (show l)]
                BlankBox ->
                  HH.li [HU.classProp "gameBoard-grid-item blank-box"]
                    []
        ]

evalClass :: LetterEval -> String
evalClass = case _ of
    NotInWord -> "eval-not-in-word"
    WrongPos  -> "eval-wrong-pos"
    RightPos  -> "eval-right-pos"

isWrongClass :: Boolean -> String
isWrongClass isWrong
    | isWrong   = "invalid-guess"
    | otherwise = "valid-guess"

data GridBox = EvalLetter LetterEval Letter
             | InputLetter Letter
             | BlankBox

layoutGrid :: BoardState -> Seq (V5 GridBox)
layoutGrid boardState = Seq.fromFoldable $
                        List.take boardState.gameSettings.guessLimit $
        evalGrid
     <> (pickerGrid List.: List.repeat (pure BlankBox))
  where
    evalGrid = flip map (List.fromFoldable boardState.guessState) \wd ->
      lift2 EvalLetter
        (evalGuess boardState.gameSettings.goalWord wd)
        wd
    pickerGrid = map (maybe BlankBox InputLetter)
        (v5FromList (List.fromFoldable boardState.typingWord)).res

_wordPicker :: Proxy "wordPicker"
_wordPicker = Proxy
