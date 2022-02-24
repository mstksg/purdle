
module Main where

import Data.Foldable
import Data.Letter
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe
import Data.Trie as Trie
import Data.Tuple
import Data.V5
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throwException, error)
import Halogen.Aff as HA
import Effect.Console as Console
import Halogen.VDom.Driver (runUI)
import Prelude
import Purdle.Types
import Purdle.UI as UI
import Purdle.WordList
import Data.Either
import Web.DOM.ParentNode as DOM

-- test with MOUTH:
-- least, trunk, pouty, mouth
-- try: cling
-- pouty, milks, grill, cling
-- bad: payer
-- leaps, pater, paper, payer
-- bad: olden
-- peats, morne, felon
main :: Effect Unit
main = HA.runHalogenAff do
  _         <- HA.awaitBody
  container <- HA.selectElement (DOM.QuerySelector "#ui")
  initWord  <- liftEffect randomAnswer
  liftEffect $ Console.log (showWord initWord)
  -- let initWord = mkV5 P A Y E R
  -- let initWord = mkV5 O L D E N
  case container of
    Nothing   -> liftEffect $ throwException (error "#ui not found")
    Just cont -> runUI UI.mainComponent initWord cont
