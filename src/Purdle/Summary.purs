
module Purdle.Summary where

import Control.Apply
import Data.FoldableWithIndex
import Data.Either
import Control.Monad.State
import Undefined
import Data.Foldable
import Data.Letter
import Data.List.Lazy as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.List.Lazy (List)
import Data.Maybe
import Data.Maybe.First
import Data.List.Lazy.NonEmpty as NE
import Data.Monoid.Additive
import Data.Newtype hiding (traverse)
import Data.Ord.Max
import Data.PositiveInt
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Set as Set
import Data.Traversable
import Data.Tuple
import Data.V5
import Prelude
import Purdle.Evaluate
import Purdle.Types

-- | Used for keyboard display
letterColors
    :: GameInfo
    -> Map Letter LetterEval
letterColors {goalWord, guessState} = Map.fromFoldableWith (<>) $
    foldMap go guessState
  where
    go :: Word -> List.List (Tuple Letter LetterEval)
    go guess = List.fromFoldable (lift2 Tuple guess res)
      where
        res = evalGuess goalWord guess

data PositionSummary = PosForbid  (Set Letter)  -- ^ what it cannot be
                     | PosRequire Letter        -- ^ what it must be

instance Semigroup PositionSummary where
    append = case _ of
      PosForbid ys -> case _ of
        PosForbid ys' -> PosForbid (ys <> ys')
        PosRequire g  -> PosRequire g
      PosRequire l -> const (PosRequire l)

newtype FreqRange = FreqRange { isExact :: Boolean, limit :: NonNegativeInt }

derive instance Newtype FreqRange _

instance Semigroup FreqRange where
    append (FreqRange r1) (FreqRange r2) = FreqRange
      { isExact: r1.isExact || r2.isExact
      , limit: max r1.limit r2.limit  -- should be the same if both are isExact
      }

addFreqRange :: FreqRange -> FreqRange -> FreqRange
addFreqRange (FreqRange r1) (FreqRange r2) = FreqRange
    { isExact: r1.isExact || r2.isExact
    , limit: r1.limit <> r2.limit
    }

sumFreqs :: List FreqRange -> Maybe FreqRange
sumFreqs xs = case List.step xs of
    List.Nil -> Nothing
    List.Cons y ys -> Just (foldl addFreqRange y ys)

-- hm...blacks and yellowCounts have to be disjoint.
type ColorSummary =
    { freqs     :: Map Letter FreqRange
    , positions :: V5 PositionSummary
    }

colorSummary
    :: GameInfo
    -> ColorSummary
colorSummary ginf =
    { freqs: summary.freqs
    , positions
    }
  where
    summary = letterSummary ginf
    Tuple blacks positions = for summary.positions \summ ->
      Tuple summ.blacks $
        case summ.greens of
          First (Just g) -> PosRequire g
          First Nothing  -> PosForbid summ.yellows

type LetterSummary =
    { positions :: V5 { blacks :: Set Letter, yellows :: Set Letter, greens :: First Letter }
    , freqs     :: Map Letter FreqRange
    }

letterSummary
    :: GameInfo
    -> LetterSummary
letterSummary {goalWord, guessState} = summary
    { freqs = unwrap summary.freqs
    }
  where
    blackLetter l = SemigroupMap $ Map.singleton l $
      List.singleton $ FreqRange { isExact: true, limit: Nothing }
    seenLetter l = SemigroupMap $ Map.singleton l $
      List.singleton $ FreqRange { isExact: false, limit: Just positiveOne }
    evalLetter l = case _ of
      NotInWord -> Tuple (blackLetter l) $ emptySpot { blacks  = Set.singleton l }
      WrongPos  -> Tuple (seenLetter l)  $ emptySpot { yellows = Set.singleton l }
      RightPos  -> Tuple (seenLetter l)  $ emptySpot { greens  = First (Just l)  }
    summary = foldMap go guessState
    emptySpot :: { blacks :: Set Letter, yellows :: Set Letter, greens :: First Letter }
    emptySpot = mempty
    go guess = { positions, freqs: SemigroupMap $ Map.mapMaybe sumFreqs freqSums }
      where
        res       = evalGuess goalWord guess
        Tuple (SemigroupMap freqSums) positions = sequence (lift2 evalLetter guess res)

gameWon :: GameInfo -> Boolean
gameWon {goalWord, guessState} = List.any (_ == goalWord) guessState

type HardModeErrors =
    { greenNotUsed  :: V5 (Maybe Letter)
    , yellowNotUsed :: Map Letter PositiveInt
    }

hardModeNoErrors :: HardModeErrors -> Boolean
hardModeNoErrors = isJust <<< showHardModeErrors

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

