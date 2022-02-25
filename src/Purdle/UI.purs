
module Purdle.UI where

import Control.Apply
import Control.Monad.State
import Data.Array as Array
import Data.Either
import Data.Foldable
import Data.Lazy
import Data.Letter
import Data.List.Lazy as List
import Data.Maybe
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Trie (Trie)
import Data.Trie as Trie
import Data.V5
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
import Effect
import Prelude
import Purdle.Evaluate
import Purdle.Summary
import Purdle.Types
import Purdle.UI.Board
import Purdle.UI.Game
import Purdle.UI.WordPicker
import Type.Proxy
import Undefined

mainComponent :: forall q o m. MonadEffect m => H.Component q Word o m
mainComponent = H.mkComponent
    { initialState: \goalWord ->
        { goalWord, gameMode: SuperHardMode, gameProgress: GPInitialized }
    , render: \gst -> HH.slot _board unit game gst identity
    , eval: H.mkEval $ H.defaultEval
        { handleAction = case _ of
            GOToast str -> liftEffect $ toast str
            GOGameOver e -> liftEffect $ toast $ case e of
              GameWin i -> "Congrats!  Won in " <> show i <> " guesses."
              GameLoss w -> showWord w
        }
    }

_game :: Proxy "game"
_game = Proxy

foreign import toast :: String -> Effect Unit
