
module Main where

-- import Corona.Chart.UI as UI
-- import Corona.Data.JHU (fetchCoronaData)
-- import Corona.Data.NYT as NYT
-- import Data.Either (Either(..))
-- import Effect.Class.Console
import Data.Maybe
import Data.Trie as Trie
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throwException, error)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude
import Purdle.Types
import Purdle.UI as UI
import Web.DOM.ParentNode as DOM

main :: Effect Unit
main = HA.runHalogenAff do
  _         <- HA.awaitBody
  container <- HA.selectElement (DOM.QuerySelector "#ui")
  case container of
    Nothing   -> liftEffect $ throwException (error "#ui not found")
    Just cont -> runUI (UI.mainComponent initialDict) unit cont

initialDict :: Dictionary
initialDict = Trie.empty
-- foreign import logMe :: forall a. a -> Effect Unit
