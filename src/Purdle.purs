
module Purdle where

import Prelude
import Data.Monoid.Additive
import Data.Newtype
import Control.Alternative
import Data.Map as Map
import Data.Maybe
import Control.Alt
import Control.Monad.State
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple
import Control.Apply
import Data.List.Lazy as List
import Data.Traversable
import Data.Maybe.First
import Data.Map (Map)

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M
            | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

derive instance Eq Letter
derive instance Ord Letter

newtype V5 a = V5
    { x1 :: a
    , x2 :: a
    , x3 :: a
    , x4 :: a
    , x5 :: a
    }

derive instance Functor V5
derive instance Eq a => Eq (V5 a)

mkV5 :: forall a. a -> a -> a -> a -> a -> V5 a
mkV5 x1 x2 x3 x4 x5 = V5 {x1, x2, x3, x4, x5}

unV5 :: forall a. V5 a -> { x1 :: a, x2 :: a, x3 :: a, x4 :: a, x5 :: a }
unV5 (V5 x) = x

instance Apply V5 where
    apply (V5 f) (V5 x) = V5
      { x1: f.x1 x.x1
      , x2: f.x2 x.x2
      , x3: f.x3 x.x3
      , x4: f.x4 x.x4
      , x5: f.x5 x.x5
      }

instance Applicative V5 where
    pure x = V5 { x1: x, x2: x, x3: x, x4: x, x5: x }

instance Foldable V5 where
    foldr f z (V5 x) = f x.x1 (f x.x2 (f x.x3 (f x.x4 (f x.x5 z))))
    foldl f z (V5 x) = f (f (f (f (f z x.x1) x.x2) x.x3) x.x4) x.x5
    foldMap f (V5 x) = f x.x1 <> f x.x2 <> f x.x3 <> f x.x4 <> f x.x5

instance Traversable V5 where
    traverse f (V5 x) = mkV5 <$> f x.x1 <*> f x.x2 <*> f x.x3 <*> f x.x4 <*> f x.x5
    sequence (V5 x) = mkV5 <$> x.x1 <*> x.x2 <*> x.x3 <*> x.x4 <*> x.x5

instance Semigroup a => Semigroup (V5 a) where
    append = lift2 append

instance Monoid a => Monoid (V5 a) where
    mempty = pure mempty

data Ix5 = Ix5_0 | Ix5_1 | Ix5_2 | Ix5_3 | Ix5_4

type Word = V5 Letter

data L6 a =
    L6_0
  | L6_1 a
  | L6_2 a a
  | L6_3 a a a
  | L6_4 a a a a
  | L6_5 a a a a a
  | L6_6 a a a a a a

derive instance Functor L6

l6ToList :: forall a. L6 a -> List.List a
l6ToList = case _ of
    L6_0 -> List.nil
    L6_1 x1 -> x1 List.: List.nil
    L6_2 x1 x2 -> x1 List.: x2 List.: List.nil
    L6_3 x1 x2 x3 -> x1 List.: x2 List.: x3 List.: List.nil
    L6_4 x1 x2 x3 x4 -> x1 List.: x2 List.: x3 List.: x4 List.: List.nil
    L6_5 x1 x2 x3 x4 x5 -> x1 List.: x2 List.: x3 List.: x4 List.: x5 List.: List.nil
    L6_6 x1 x2 x3 x4 x5 x6 -> x1 List.: x2 List.: x3 List.: x4 List.: x5 List.: x5 List.: List.nil

instance foldableL6 :: Foldable L6 where
    foldMap f = foldMap f <<< l6ToList
    foldr f z = foldr f z <<< l6ToList
    foldl f z = foldl f z <<< l6ToList

consL6 :: forall a. a -> L6 a -> Maybe (L6 a)
consL6 y = case _ of
    L6_0 -> Just $ L6_1 y
    L6_1 x1 -> Just $ L6_2 y x1
    L6_2 x1 x2 -> Just $ L6_3 y x1 x2
    L6_3 x1 x2 x3 -> Just $ L6_4 y x1 x2 x3
    L6_4 x1 x2 x3 x4 -> Just $ L6_5 y x1 x2 x3 x4
    L6_5 x1 x2 x3 x4 x5 -> Just $ L6_6 y x1 x2 x3 x4 x5
    L6_6 _  _  _  _  _  _  -> Nothing

lastL6 :: forall a. L6 a -> Maybe a
lastL6 = case _ of
    L6_0 -> Nothing
    L6_1 x1 -> Just x1
    L6_2 _  x2 -> Just x2
    L6_3 _  _  x3 -> Just x3
    L6_4 _  _  _  x4 -> Just x4
    L6_5 _  _  _  _  x5 -> Just x5
    L6_6 _  _  _  _  _  x6 -> Just x6

type GuessState = L6 Word

type GameInfo =
    { goalWord :: Word
    , guessState :: GuessState
    }

data Color = Black | Yellow | Green

derive instance Eq Color
derive instance Ord Color

instance Show Color where
    show = case _ of
      Black  -> "Black"
      Yellow -> "Yellow"
      Green  -> "Green"

instance Semigroup Color where
    append = max

-- | Goal and guess
evalGuess :: Word -> Word -> V5 Color
evalGuess goal guess = evalState (sequence (lift2 seekColors goal guess)) yellowFreqs
  where
    yellowFreqs :: Map Letter Int
    yellowFreqs = Map.fromFoldableWith (+) $
      lift2 (\gl gu -> Tuple gl (if gl == gu then 0 else 1)) goal guess
    seekColors :: Letter -> Letter -> State (Map Letter Int) Color
    seekColors gl gu
      | gl == gu  = pure Green
      | otherwise = do
          freq <- gets $ fromMaybe 0 <<< Map.lookup gu
          if freq > 0
            then Yellow <$ modify (Map.update (Just <<< (_ - 1)) gu)
            else pure Black

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

type ColorSummary =
    { blacks  :: Set Letter
    , yellows :: Map Letter Int
    , greens  :: V5 (Maybe Letter)
    }

colorSummary
    :: GameInfo
    -> ColorSummary
colorSummary {goalWord, guessState} = summary
    { yellows = map unwrap (unwrap summary.yellows)
    , greens  = map unwrap summary.greens
    }
  where
    summary = foldMap go (l6ToList guessState)
    buildBY g = case _ of
      Black  -> { blacks: Set.singleton g, yellows: mempty }
      Yellow -> { blacks: Set.empty, yellows: Map.SemigroupMap $ Map.singleton g (Additive 1) }
      Green  -> mempty
    go guess =
        { blacks
        , yellows
        , greens: lift2 (\gw r -> First (gw <$ guard (r == Green))) goalWord res
        }
      where
        res = evalGuess goalWord guess
        { blacks, yellows } = fold (List.fromFoldable (lift2 buildBY guess res))

positionSummary
    :: GameInfo
    -> V5 { blacks :: Set Letter, yellows :: Set Letter, greens :: First Letter }
positionSummary {goalWord, guessState} = foldMap go (l6ToList guessState)
  where
    emptySpot :: { blacks :: Set Letter, yellows :: Set Letter, greens :: First Letter }
    emptySpot = mempty
    go guess = lift2 evalLetter guess res
      where
        res = evalGuess goalWord guess
        evalLetter l = case _ of
          Black  -> emptySpot { blacks  = Set.singleton l }
          Yellow -> emptySpot { yellows = Set.singleton l }
          Green  -> emptySpot { greens  = First (Just l)  }

gameWon :: GameInfo -> Boolean
gameWon {goalWord, guessState} = List.any (_ == goalWord) (l6ToList guessState)

-- validHardMode :: GuessState -> Word -> Boolean
-- validHardMode = _

-- data GuessRes
--     = WrongGuess GuessState
--     | RightGuess GuessState
--     |

makeGuess
    :: GameInfo
    -> Word
    -> Maybe { won :: Boolean, guessState :: GuessState }
makeGuess { goalWord, guessState } guess = map addWin (consL6 guess guessState)
  where
    addWin gs = { guessState: gs, won: guess == goalWord }



