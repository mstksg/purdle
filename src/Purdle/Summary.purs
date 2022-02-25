
module Purdle.Summary where

import Control.Apply
import Control.Monad.State
import Data.Either
import Data.Foldable
import Data.FoldableWithIndex
import Data.Letter
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.List.Lazy.NonEmpty as NE
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe
import Data.Maybe.First
import Data.Monoid.Additive
import Data.Newtype hiding (traverse)
import Data.Ord.Max
import Data.PositiveInt
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Traversable
import Data.Tuple
import Data.V5
import Prelude
import Purdle.Evaluate
import Purdle.Types
import Undefined

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
derive instance Eq FreqRange
derive instance Ord FreqRange

instance Show FreqRange where
    show (FreqRange fr) = "(FreqRange " <> show fr <> ")"

instance Semigroup FreqRange where
    append (FreqRange r1) (FreqRange r2) = FreqRange
      { isExact: r1.isExact || r2.isExact
      , limit: max r1.limit r2.limit  -- should be the same if both are isExact
      }

mkFreqRange :: Boolean -> NonNegativeInt -> FreqRange
mkFreqRange isExact limit = FreqRange {isExact, limit}

addFreqRange :: FreqRange -> FreqRange -> FreqRange
addFreqRange (FreqRange r1) (FreqRange r2) = FreqRange
    { isExact: r1.isExact || r2.isExact
    , limit: r1.limit <> r2.limit
    }

sumFreqs :: List FreqRange -> Maybe FreqRange
sumFreqs xs = case List.step xs of
    List.Nil -> Nothing
    List.Cons y ys -> Just (foldl addFreqRange y ys)

mkEvals :: GameInfo -> List { word :: Word, eval :: V5 LetterEval }
mkEvals {goalWord, guessState} =
          (\word -> { word, eval: evalGuess goalWord word })
      <$> List.fromFoldable guessState

type ColorSummary =
    { freqs     :: Map Letter FreqRange
    , positions :: V5 PositionSummary
    }

colorSummary
    :: GameInfo
    -> ColorSummary
colorSummary = evalsToColorSummary <<< mkEvals

evalsToColorSummary
    :: List { word :: Word, eval :: V5 LetterEval }
    -> ColorSummary
evalsToColorSummary xs =
    { freqs: summary.freqs
    , positions
    }
  where
    summary = evalsToLetterSummary xs
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
letterSummary = evalsToLetterSummary <<< mkEvals

evalsToLetterSummary :: List { word :: Word, eval :: V5 LetterEval } -> LetterSummary
evalsToLetterSummary xs = summary
    { freqs = unwrap summary.freqs }
  where
    summary = flip foldMap xs \{word, eval} ->
      let { positions, freqs } = evalToLetterSummary word eval
      in  { positions, freqs: SemigroupMap freqs }

evalToLetterSummary :: Word -> V5 LetterEval -> LetterSummary
evalToLetterSummary guess res = { positions, freqs: Map.mapMaybe sumFreqs freqSums }
  where
    blackLetter l = SemigroupMap $ Map.singleton l $
      List.singleton $ FreqRange { isExact: true, limit: Nothing }
    seenLetter l = SemigroupMap $ Map.singleton l $
      List.singleton $ FreqRange { isExact: false, limit: Just positiveOne }
    emptySpot :: { blacks :: Set Letter, yellows :: Set Letter, greens :: First Letter }
    emptySpot = mempty
    evalLetter l = case _ of
      NotInWord -> Tuple (blackLetter l) $ emptySpot { blacks  = Set.singleton l }
      WrongPos  -> Tuple (seenLetter l)  $ emptySpot { yellows = Set.singleton l }
      RightPos  -> Tuple (seenLetter l)  $ emptySpot { greens  = First (Just l)  }
    Tuple (SemigroupMap freqSums) positions = sequence (lift2 evalLetter guess res)

