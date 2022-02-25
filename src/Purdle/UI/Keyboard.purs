
module Purdle.UI.Keyboard where

import Control.Alternative
import Control.Monad.State.Class
import Data.Foldable
import Data.Letter
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.String as String
import Effect.Class
import Halogen as H
import Halogen.Aff.Util as HU
import Halogen.HTML as HH
import Halogen.HTML.Core as HH
import Halogen.HTML.Elements as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Halogen.Query.Event as HQ
import Halogen.Util as HU
import Prelude
import Undefined
import Web.DOM.Document as Document
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as Keyboard
import Web.UIEvent.KeyboardEvent.EventTypes as Keyboard

data KeyboardQuery a = KQSetColors ColorMap a

data Color = Black | Yellow | Green

type ColorMap = Map Letter Color

data KeyEvent = KeyEventKey Key
              | KeyEventInit
              | KeyEventFinal

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

keyboard :: forall m. MonadEffect m => H.Component KeyboardQuery ColorMap Key m
keyboard = H.mkComponent
    { initialState: \cm -> { keyListener: Nothing, colorMap: cm }
    , render: \kst ->
        HH.div [ HU.classProp "keyboard" ] $
          flip map keyGrid \keyRow ->
            HH.div [HU.classProp "keyboard-row"] $
              flip map keyRow \k ->
                HH.button [
                    HU.classProp $ "keyboard-key "
                                <> keyClass k <> " "
                                <> fromMaybe "key-grey" (colorClass kst.colorMap k)
                  , HP.type_ HP.ButtonButton
                  , HE.onClick \_ -> KeyEventKey k
                  ]
                  [HH.text (showKey k)]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = case _ of
            KeyEventKey k -> H.raise k
            KeyEventInit  -> do
              doc <- liftEffect $
                    map (Document.toEventTarget <<< HTMLDocument.toDocument)
                <<< Window.document
                =<< HTML.window
              sId <- H.subscribe $ HQ.eventListener
                Keyboard.keydown
                    doc
                    (map KeyEventKey <<< processKeyEvent <=< Keyboard.fromEvent)
              modify_ \kst -> kst { keyListener = Just sId }
            KeyEventFinal -> do
              kl <- gets _.keyListener
              traverse_ H.unsubscribe kl
              modify_ \kst -> kst { keyListener = Nothing }
        , handleQuery = case _ of
            KQSetColors cmap r -> Just r <$ modify_ (\kst -> kst { colorMap = cmap })
        , initialize = Just KeyEventInit
        , finalize = Just KeyEventFinal
        }
    }

processKeyEvent :: Keyboard.KeyboardEvent -> Maybe Key
processKeyEvent ke = case Keyboard.key ke of
    "Enter" -> Just EnterKey
    "Backspace" -> Just BackspaceKey
    ks -> LetterKey <$> do
      {head, tail} <- String.uncons ks
      guard $ tail == ""
      head `Map.lookup` lookupLetterMap
