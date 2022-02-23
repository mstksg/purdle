
module Purdle.UI.Keyboard where

import Control.Monad.State.Class
import Data.Letter
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.String as String
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff.Util as HU
import Halogen.HTML as HH
import Halogen.HTML.Core as HH
import Halogen.HTML.Elements as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Halogen.Query.Event as HQE
import Halogen.Util as HU
import Prelude
import Undefined
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data KeyboardQuery a = KQSetColors ColorMap a

data Color = Black | Yellow | Green

type ColorMap = Map Letter Color

data Key = LetterKey Letter
         | BackspaceKey
         | EnterKey

data KeyOut = Init
            | HandleKey Key

showKey :: Key -> String
showKey = case _ of
    LetterKey l -> show l
    BackspaceKey -> "Back"
    EnterKey -> "Enter"

readLetter :: String -> Maybe Letter
readLetter char = case String.toUpper char of
  "A" -> Just A
  "B" -> Just B
  "C" -> Just C
  "D" -> Just D
  "E" -> Just E
  "F" -> Just F
  "G" -> Just G
  "H" -> Just H
  "I" -> Just I
  "J" -> Just J
  "K" -> Just K
  "L" -> Just L
  "M" -> Just M
  "N" -> Just N
  "O" -> Just O
  "P" -> Just P
  "Q" -> Just Q
  "R" -> Just R
  "S" -> Just S
  "T" -> Just T
  "U" -> Just U
  "V" -> Just V
  "W" -> Just W
  "X" -> Just X
  "Y" -> Just Y
  "Z" -> Just Z
  _ -> Nothing

keyClass :: Key -> String
keyClass = case _ of
    LetterKey _ -> ""
    BackspaceKey -> "wide-key"
    EnterKey -> "wide-key"

keyGrid :: Array (Array Key)
keyGrid =
  [ map LetterKey [Q,W,E,R,T,Y,U,I,O,P]
  , map LetterKey [A,S,D,F,G,H,J,K,L]
  , [EnterKey] <> map LetterKey [Z,X,C,V,B,N,M] <> [BackspaceKey]
  ]

colorClass :: ColorMap -> Key -> Maybe String
colorClass cmap = case _ of
    LetterKey l -> flip map (Map.lookup l cmap) case _ of
                     Black  -> "key-black"
                     Yellow -> "key-yellow"
                     Green  -> "key-green"
    _           -> Nothing

keyboard :: forall m. MonadEffect m => H.Component KeyboardQuery ColorMap Key m
keyboard = H.mkComponent
    { initialState: identity
    , render: \cmap ->
        HH.div [HU.classProp "keyboard"] $
          flip map keyGrid \keyRow ->
            HH.div [HU.classProp "keyboard-row"] $
              flip map keyRow \k ->
                HH.button [
                    HU.classProp $ "keyboard-key "
                                <> keyClass k <> " "
                                <> fromMaybe "key-grey" (colorClass cmap k)
                  , HP.type_ HP.ButtonButton
                  , HE.onClick \_ -> HandleKey k
                  ]
                  [HH.text (showKey k)]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = case _ of
              Init -> do
                let
                  fromKeyboardEvent e = case KE.key e of
                    "Enter" -> Just EnterKey
                    "Backspace" -> Just BackspaceKey
                    letter -> LetterKey <$> readLetter letter
                document <- H.liftEffect $ Window.document =<< HTML.window
                void $ H.subscribe $
                  HQE.eventListener
                    KET.keyup
                    (HTMLDocument.toEventTarget document)
                    (map HandleKey <=< map fromKeyboardEvent <<< KE.fromEvent)
              HandleKey key -> H.raise key
        , handleQuery = case _ of
            KQSetColors cmap r -> Just r <$ put cmap
        , initialize = Just Init
        }
    }

