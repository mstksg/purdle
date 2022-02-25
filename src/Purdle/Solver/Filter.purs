
module Purdle.Solver.Filter where

import Control.Apply
import Data.Array as Array
import Data.Foldable
import Data.FunctorWithIndex
import Data.Lazy
import Data.Letter
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe
import Data.PositiveInt
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable
import Data.Trie (Trie)
import Data.Trie as Trie
import Data.Tuple
import Data.V5
import Prelude
import Purdle.Evaluate
import Purdle.Summary
import Purdle.Types
import Purdle.WordList

data Clue =
    CRequirePos Ix5 Letter        -- ^ should never have two letters for same ix
  | CForbidPos Ix5 Letter
  | CLetterFreq Letter FreqRange -- ^ should never have two "exact" for same letter, also lim is between 0 and 5

derive instance Eq Clue
derive instance Ord Clue

instance Show Clue where
    show = case _ of
      CRequirePos i l -> "(CRequirePos " <> show i <> " " <> show l <> ")"
      CForbidPos i l -> "(CForbidPos " <> show i <> " " <> show l <> ")"
      CLetterFreq l r -> "(CLetterFreq " <> show l <> " " <> show r <> ")"

applyClue :: Clue -> Word -> Boolean
applyClue cl w = case cl of
    CRequirePos i l -> ixV5 i w == l
    CForbidPos  i l -> ixV5 i w /= l
    CLetterFreq l (FreqRange { isExact, limit }) ->
      let fr = List.length <<< List.filter (_ == l) <<< List.fromFoldable $ w
      in  if isExact
            then fr == unNonNegative limit
            else fr >= unNonNegative limit

-- | the set of clues that are mutually exclusive to the given clue
clueExcludes :: Clue -> Set Clue
clueExcludes = case _ of
    CRequirePos i l ->
        Set.insert (CLetterFreq l (FreqRange { isExact: true, limit: Nothing }))
      $ Set.insert (CForbidPos i l)
      $ Set.map (CRequirePos i) (l `Set.delete` allLettersSet)
    CForbidPos  i l -> Set.singleton (CRequirePos i l)
    CLetterFreq l (FreqRange { isExact, limit })
      | isExact ->
          Set.map (\i -> CLetterFreq l (FreqRange { isExact, limit: i }))
            (limit `Set.delete` validNonNegative)
       <> (if isNothing limit
            then Set.fromFoldable (map (\i -> CRequirePos i l) v5Ixes)
            else Set.empty
          )
      | otherwise -> Set.empty

validNonNegative :: Set NonNegativeInt
validNonNegative = Set.fromFoldable $ Array.mapMaybe toNonNegative $
    [0,1,2,3,4,5]

allClues :: Set Clue
allClues = Set.fromFoldable $ foldr (<>) List.nil $
    [ CRequirePos <$> ix5list <*> letterlist
    , CForbidPos  <$> ix5list <*> letterlist
    , do letter  <- letterlist
         isExact <- false List.: true List.: List.nil
         limit   <- nonNegativelist
         pure $ CLetterFreq letter (FreqRange {isExact, limit})
    ]
  where
    ix5list = List.fromFoldable v5Ixes
    letterlist = List.fromFoldable allLetters
    nonNegativelist = List.fromFoldable validNonNegative

-- | Useful for building clues off of multiple words at a time
colorSummaryToClues :: ColorSummary -> Set Clue
colorSummaryToClues {freqs, positions} = posClues <> Set.fromFoldable freqClues
  where
    mkPosClue i = case _ of
      PosForbid  ls -> Set.map (CForbidPos i) ls
      PosRequire l  -> Set.singleton (CRequirePos i l)
    posClues = fold (lift2 mkPosClue v5Ixes positions)
    freqClues :: List Clue
    freqClues = map (\(Tuple l fr) -> CLetterFreq l fr ) (Map.toUnfoldable freqs)

evalsToClues :: List { word :: Word, eval :: V5 LetterEval } -> Set Clue
evalsToClues = colorSummaryToClues <<< evalsToColorSummary

-- | Useful for step-by-step building clues with each new word
cluesForWord :: Word -> V5 LetterEval -> List Clue
cluesForWord guess res = List.fromFoldable posClues <> freqClues
  where
    blackLetter l = SemigroupMap $ Map.singleton l $
      List.singleton $ FreqRange { isExact: true, limit: Nothing }
    seenLetter l = SemigroupMap $ Map.singleton l $
      List.singleton $ FreqRange { isExact: false, limit: Just positiveOne }
    evalLetter :: Ix5 -> Letter -> LetterEval -> Tuple (SemigroupMap Letter (List FreqRange)) Clue
    evalLetter i l = case _ of
      NotInWord -> Tuple (blackLetter l) (CForbidPos  i l)
      WrongPos  -> Tuple (seenLetter  l) (CForbidPos  i l)
      RightPos  -> Tuple (seenLetter  l) (CRequirePos i l)
    Tuple (SemigroupMap freqSums) posClues = sequence (lift3 evalLetter v5Ixes guess res)
    freqClues = map (\(Tuple l fr) -> CLetterFreq l fr )
            <<< Map.toUnfoldable
              $ Map.mapMaybe sumFreqs freqSums

mkClueTrie :: Array Word -> Trie Clue (Set Word)
mkClueTrie wordList = Trie.unfold go { words: Set.fromFoldable wordList, clues: allClues }
  where
    go  :: { clues :: Set Clue, words :: Set Word }
        -> { here :: Maybe (Set Word), there :: Map Clue (Lazy ({clues :: Set Clue, words :: Set Word }))}
    go {clues, words} =
      { here: Just words
      , there: flip mapWithIndex (Set.toMap clues) \cl _ -> defer \_ ->
          { clues: (cl `Set.delete` clues) `Set.difference` clueExcludes cl
          , words: Set.filter (applyClue cl) words
          }
      }

allowedWordsClueTrie :: Trie Clue (Set Word)
allowedWordsClueTrie = mkClueTrie allowedWords

allowedAnswersClueTrie :: Trie Clue (Set Word)
allowedAnswersClueTrie = mkClueTrie allowedAnswers
