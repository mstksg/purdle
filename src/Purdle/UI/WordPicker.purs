
module Purdle.UI.WordPicker where

import Control.Monad.State.Class
import Data.Letter
import Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Prelude
import Purdle.UI.Keyboard
import Type.Proxy

data WordPickerQuery a = WPQSetColors ColorMap a
                       | WPQReset a

data WordPickerOut = WPQInProgress (Seq Letter)
                   | WPQSubmit (Seq Letter)

wordPicker :: forall m. MonadEffect m => Int -> H.Component WordPickerQuery (Seq Letter) WordPickerOut m
wordPicker wordSize = H.mkComponent
    { initialState: Seq.fromFoldable
    , render: \_ ->
        HH.slot _keyboard unit keyboard Map.empty identity
    , eval: H.mkEval $ H.defaultEval
        { handleAction = case _ of
            LetterKey l  -> do
              sq <- get
              when (Seq.length sq < wordSize) do
                let sq' = sq `Seq.snoc` l
                H.raise $ WPQInProgress sq'
                put sq'
            EnterKey     -> H.raise =<< gets WPQSubmit
            BackspaceKey -> H.raise <<< WPQInProgress
                        =<< modify (fromMaybe Seq.empty <<< Seq.init)
        , handleQuery  = case _ of
            WPQSetColors cm r -> H.query _keyboard unit $ KQSetColors cm r
            WPQReset r -> Just r <$ do
               put Seq.empty
               H.raise $ WPQInProgress Seq.empty
        }
    }

_keyboard :: Proxy "keyboard"
_keyboard = Proxy
