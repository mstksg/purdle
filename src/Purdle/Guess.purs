
module Purdle.Guess where

import Purdle.Types
import Data.Maybe
import Data.Either
import Prelude
import Purdle.Summary
import Data.L6

makeGuess
    :: GameInfo
    -> Word
    -> Maybe { won :: Boolean, guessState :: GuessState }
makeGuess { goalWord, guessState } guess = map addWin (consL6 guess guessState)
  where
    addWin gs = { guessState: gs, won: guess == goalWord }

makeGuessHardMode
    :: GameInfo
    -> Word
    -> Maybe (Either HardModeErrors { won :: Boolean, guessState :: GuessState })
makeGuessHardMode ginf@{ goalWord, guessState } guess = map addEval (consL6 guess guessState)
  where
    addEval gs = if hardModeNoErrors errs
        then Right { guessState: gs, won: guess == goalWord }
        else Left errs
      where
        summary = colorSummary ginf
        errs    = validHardMode summary guess

makeGuessSuperHardMode
    :: GameInfo
    -> Word
    -> Maybe (Either SuperHardModeErrors { won :: Boolean, guessState :: GuessState })
makeGuessSuperHardMode ginf@{ goalWord, guessState } guess = map addEval (consL6 guess guessState)
  where
    addEval gs = if superHardModeNoErrors errs
        then Right { guessState: gs, won: guess == goalWord }
        else Left errs
      where
        summary = colorSummary ginf
        errs    = validSuperHardMode summary guess