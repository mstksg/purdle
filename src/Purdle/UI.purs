
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
import Prelude
import Purdle.Evaluate
import Purdle.Summary
import Purdle.Types
import Purdle.UI.Board
import Purdle.UI.WordPicker
import Type.Proxy
import Undefined


mainComponent :: forall q i o m. MonadEffect m => Dictionary -> H.Component q i o m
mainComponent dict = H.mkComponent
    { initialState: const unit
    , render: \_ -> HH.slot _board unit (board dict) initialSettings identity
    , eval: H.mkEval $ H.defaultEval
        { handleAction = case _ of
            BOToast str -> liftEffect $ Console.log str
            BOEndGame i -> liftEffect $ Console.log "game over"
        }
    }
  where
    initialSettings =
      { gameMode: NormalMode
      , goalWord: mkV5 H E L L O
      , guessLimit: 6
      }

_board :: Proxy "board"
_board = Proxy
