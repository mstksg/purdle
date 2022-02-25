
module Purdle.Validate where

import Control.Monad.State
import Data.PositiveInt
import Data.Traversable
import Data.V5
import Control.Apply
import Data.Newtype (unwrap)
import Data.Either
import Data.Letter
import Data.List.Lazy as List
import Data.List.Lazy.NonEmpty as NE
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple
import Prelude
import Purdle.Summary
import Purdle.Types

type HardModeErrors =
    { greenNotUsed  :: V5 (Maybe Letter)
    , yellowNotUsed :: Map Letter PositiveInt
    }

cardinalize :: Int -> String
cardinalize i
    | i == 1    = "1st"
    | i == 2    = "2nd"
    | i == 3    = "3rd"
    | otherwise = show i <> "th"

showHardModeErrors :: HardModeErrors -> Maybe (NE.NonEmptyList String)
showHardModeErrors { greenNotUsed, yellowNotUsed } =
    NE.fromList $ greenErrors <> yellowErrors
  where
    greenErrors = List.catMaybes $
      List.zipWith (\i -> map $ \l ->
                        cardinalize i <> " letter must be " <> show l
                   )
        (List.iterate (_ + 1) 1)
        (List.fromFoldable greenNotUsed)
    yellowErrors = map (\(Tuple l i) -> "Guess missing " <> show (unPositive i) <> "x " <> show l)
                 $ Map.toUnfoldable yellowNotUsed

validHardMode :: ColorSummary -> Word -> HardModeErrors
validHardMode { freqs, positions } guess =
    { greenNotUsed, yellowNotUsed }
  where
    go :: Letter -> PositionSummary -> State (Map Letter PositiveInt) (Maybe Letter)
    go gu pos = do
      modify_ (Map.update decrementPositive gu)
      pure case pos of
        PosForbid _   -> Nothing
        PosRequire gr
          | gu == gr  -> Nothing
          | otherwise -> Just gr
    Tuple greenNotUsed rawYellowNotUsed =
        runState (sequence (lift2 go guess positions))
                 (Map.mapMaybe (_.limit <<< unwrap) freqs)
    -- get rid of double-counted
    yellowNotUsed = foldl
      (\mp -> case _ of
          Nothing -> mp
          Just l  -> Map.update decrementPositive l mp
      )
      rawYellowNotUsed
      greenNotUsed

data PositionError = IsRequired Letter | IsForbidden Letter

type SuperHardModeErrors =
    { positionErrors  :: V5 (Maybe PositionError)
    , frequencyErrors :: Map Letter (Either PositiveInt PositiveInt)
    }

superHardModeNoErrors :: SuperHardModeErrors -> Boolean
superHardModeNoErrors = isJust <<< showSuperHardModeErrors

showSuperHardModeErrors :: SuperHardModeErrors -> Maybe (NE.NonEmptyList String)
showSuperHardModeErrors {positionErrors, frequencyErrors} =
    NE.fromList $ posErrors <> freqErrors
  where
    posErrors = List.catMaybes $
      List.zipWith (\i -> map case _ of
                        IsRequired  l -> cardinalize i <> " letter must be " <> show l
                        IsForbidden l -> cardinalize i <> " letter must not be " <> show l
                   )
        (List.iterate (_ + 1) 1)
        (List.fromFoldable positionErrors)
    showFreqErrors (Tuple l e) = case e of
      Left  i -> "Used " <> show (unPositive i) <> "x too many " <> show l
      Right i -> "Missing " <> show (unPositive i) <> "x " <> show l
    freqErrors = map showFreqErrors (Map.toUnfoldable frequencyErrors)

validSuperHardMode :: ColorSummary -> Word -> SuperHardModeErrors
validSuperHardMode {freqs, positions} guess =
    { positionErrors, frequencyErrors }
  where
    go  :: Letter
        -> PositionSummary
        -> State (Map Letter Int) (Maybe PositionError)
    go gu pos = do
      modify_ $ Map.update (Just <<< (_ - 1)) gu
      pure case pos of
        PosForbid forbs
          | gu `Set.member` forbs -> Just $ IsForbidden gu
          | otherwise             -> Nothing
        PosRequire gr
          | gu == gr  -> Nothing
          | otherwise -> Just $ IsRequired gr
    Tuple positionErrors leftoverFreqs =
        runState
            (sequence (lift2 go guess positions))
            (map (unNonNegative <<< _.limit <<< unwrap) freqs)
    rawFrequencyErrors = Map.catMaybes $
      Map.intersectionWith
        (\(FreqRange fr) -> if fr.isExact then classifyInt else map Right <<< toPositive)
        freqs
        leftoverFreqs
    -- get rid of double-counted
    frequencyErrors = foldl
        (\mp -> case _ of
            Nothing              -> mp
            Just (IsForbidden l) -> mp
            Just (IsRequired l)  ->
              Map.update (traverse decrementPositive) l mp
        )
        rawFrequencyErrors
        positionErrors

