
module Purdle.UI.Keyboard where

import Control.Monad.State.Class
import Data.Letter
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
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
import Undefined

data KeyboardQuery a = KQSetColors ColorMap a

data Color = Black | Yellow | Green

type ColorMap = Map Letter Color

data Key = LetterKey Letter
         | BackspaceKey
         | EnterKey

showKey :: Key -> String
showKey = case _ of
    LetterKey l -> show l
    BackspaceKey -> "Back"
    EnterKey -> "Enter"

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

keyboard :: forall m. H.Component KeyboardQuery ColorMap Key m
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
                  , HE.onClick \_ -> k
                  ]
                  [HH.text (showKey k)]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = H.raise
        , handleQuery = case _ of
            KQSetColors cmap r -> Just r <$ put cmap
        }
    }

