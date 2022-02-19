
module Purdle.Summary where

import Control.Apply
import Purdle.Evaluate
import Data.Ord.Max
import Control.Monad.State
import Data.Foldable
import Data.L6
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.Maybe.First
import Data.Monoid.Additive
import Data.Newtype
import Data.PositiveInt
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable
import Data.Tuple
import Data.V5
import Prelude
import Purdle.Types


-- | Used for keyboard display
letterColors
    :: GameInfo
    -> Map Letter Color
letterColors {goalWord, guessState} = Map.fromFoldableWith (<>) $
    foldMap go (l6ToList guessState)
  where
    go :: Word -> List.List (Tuple Letter Color)
    go guess = List.fromFoldable (lift2 Tuple guess res)
      where
        res = evalGuess goalWord guess

-- hm...blacks and yellowCounts have to be disjoint.
type ColorSummary =
    { blacks          :: Set Letter
    , yellowCounts    :: Map Letter (Max PositiveInt)
    , yellowPositions :: V5 (Set Letter)
    , greens          :: V5 (First Letter)
    }

colorSummary
    :: GameInfo
    -> ColorSummary
colorSummary ginf =
    { blacks
    , yellowCounts: summary.yellowCounts
    , yellowPositions: map _.yellows positions
    , greens: map _.greens positions
    }
  where
    summary = letterSummary ginf
    Tuple blacks positions = for summary.positions \summ ->
      Tuple summ.blacks
        { yellows: summ.yellows
        , greens: summ.greens
        }

type LetterSummary =
    { positions    :: V5 { blacks :: Set Letter, yellows :: Set Letter, greens :: First Letter }
    , yellowCounts :: Map Letter (Max PositiveInt)
    }

letterSummary
    :: GameInfo
    -> LetterSummary
letterSummary {goalWord, guessState} = summary
    { yellowCounts = unwrap summary.yellowCounts
    }
  where
    evalLetter l = case _ of
      Black  -> Tuple mempty $ emptySpot { blacks  = Set.singleton l }
      Yellow -> Tuple (Map.SemigroupMap (Map.singleton l (Additive one))) $
                  emptySpot { yellows = Set.singleton l }
      Green  -> Tuple mempty $ emptySpot { greens  = First (Just l)  }
    summary = foldMap go (l6ToList guessState)
    emptySpot :: { blacks :: Set Letter, yellows :: Set Letter, greens :: First Letter }
    emptySpot = mempty
    go guess = { positions, yellowCounts: map (Max <<< unwrap) yellowSums }
      where
        res = evalGuess goalWord guess
        Tuple yellowSums positions = sequence (lift2 evalLetter guess res)

gameWon :: GameInfo -> Boolean
gameWon {goalWord, guessState} = List.any (_ == goalWord) (l6ToList guessState)

type HardModeErrors =
    { greenNotUsed  :: V5 (Maybe Letter)
    , yellowNotUsed :: Map Letter PositiveInt
    }

hardModeNoErrors :: HardModeErrors -> Boolean
hardModeNoErrors {greenNotUsed, yellowNotUsed} =
     all isNothing greenNotUsed
  && Map.isEmpty yellowNotUsed

validHardMode :: ColorSummary -> Word -> HardModeErrors
validHardMode {yellowCounts, greens} guess =
    { greenNotUsed, yellowNotUsed }
  where
    go :: Letter -> First Letter -> State (Map Letter PositiveInt) (Maybe Letter)
    go gu isGreen = case unwrap isGreen of
      Nothing -> Nothing <$ modify_ (Map.update decrementPositive gu)
      Just gr
        | gu == gr  -> pure Nothing
        | otherwise -> pure (Just gr)
    Tuple greenNotUsed yellowNotUsed =
        runState (sequence (lift2 go guess greens)) (map unwrap yellowCounts)

data PositionError = NeedsGreen Letter | HasYellow Letter

type SuperHardModeErrors =
    { positionErrors :: V5 (Maybe PositionError)
    , yellowNotUsed  :: Map Letter PositiveInt
    , blacksUsed     :: Set Letter
    }

superHardModeNoErrors :: SuperHardModeErrors -> Boolean
superHardModeNoErrors { positionErrors, yellowNotUsed, blacksUsed } =
     all isNothing positionErrors
  && Map.isEmpty yellowNotUsed
  && Set.isEmpty blacksUsed

validSuperHardMode :: ColorSummary -> Word -> SuperHardModeErrors
validSuperHardMode {blacks, yellowCounts, yellowPositions, greens} guess =
    { positionErrors, yellowNotUsed, blacksUsed }
  where
    go  :: Letter
        -> First Letter
        -> Set Letter
        -> State {yellowNotUsed :: Map Letter PositiveInt, blacksUsed :: Set Letter} (Maybe PositionError)
    go gu isGreen yellowsHere = case unwrap isGreen of
      Nothing
        | gu `Set.member` blacks -> do
            modify_ $ \errs ->
              errs { blacksUsed = Set.insert gu errs.blacksUsed }
            pure Nothing
        | otherwise              -> do
            modify_ $ \errs -> errs
              { yellowNotUsed = Map.update decrementPositive gu errs.yellowNotUsed }
            pure if gu `Set.member` yellowsHere
              then Just (HasYellow gu)
              else Nothing
      Just gr
        | gu == gr  -> pure Nothing
        | otherwise -> pure (Just (NeedsGreen gr))
    Tuple positionErrors {yellowNotUsed, blacksUsed} =
        runState
            (sequence (lift3 go guess greens yellowPositions))
            { yellowNotUsed: map unwrap yellowCounts, blacksUsed: Set.empty }

