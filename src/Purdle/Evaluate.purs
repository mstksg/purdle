
module Purdle.Evaluate where

import Control.Apply
import Control.Monad.State
import Data.Letter
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.PositiveInt
import Data.Traversable
import Data.Tuple
import Data.V5
import Prelude
import Purdle.Types

-- | Goal and guess
evalGuess :: Word -> Word -> V5 Color
evalGuess goal guess = evalState (sequence (lift2 seekColors goal guess)) yellowFreqs
  where
    yellowFreqs :: Map Letter PositiveInt
    yellowFreqs = Map.fromFoldableWith (+) $
      lift2 (\gl gu -> Tuple gl (if gl == gu then zero else one)) goal guess
    seekColors :: Letter -> Letter -> State (Map Letter PositiveInt) Color
    seekColors gl gu
      | gl == gu  = pure Green
      | otherwise = do
          freq <- gets (Map.lookup gu)
          case freq of
            Nothing -> pure Black
            Just fr -> Yellow <$ modify (Map.update decrementPositive gu)

