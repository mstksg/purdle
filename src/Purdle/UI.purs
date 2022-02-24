
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
import Purdle.UI.WordPicker
import Type.Proxy
import Undefined


mainComponent :: forall q o m. MonadEffect m => H.Component q Word o m
mainComponent = H.mkComponent
    { initialState: \w -> { gameMode: SuperHardMode, goalWord: w, guessLimit: 6 }
    , render: \settings -> HH.slot _board unit board settings identity
    , eval: H.mkEval $ H.defaultEval
        { handleAction = case _ of
            BOToast str -> liftEffect $ toast str
            BOMadeGuess _ _ _ -> pure unit
            BOEndGame e -> liftEffect $ toast $ case e of
              GameWin i -> "Congrats!  Won in " <> show i <> " guesses."
              GameLoss w -> showWord w
        }
    }

_board :: Proxy "board"
_board = Proxy

foreign import toast :: String -> Effect Unit
