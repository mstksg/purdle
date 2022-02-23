
module Purdle.WordList where

import Data.Array as Array
import Data.Either
import Data.Letter
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe
import Data.String as String
import Data.Traversable
import Data.Trie as Trie
import Data.Tuple
import Data.V5
import Effect
import Effect.Exception (throwException, error)
import Effect.Random
import Prelude
import Purdle.Types

parseWord :: String -> Maybe Word
parseWord str = do
    chrWrd <- v5FromListExact $ String.toCodePointArray (String.trim str)
    traverse (_ `Map.lookup` lookupLetterMap) chrWrd

wordListToDictionary :: Array Word -> Dictionary
wordListToDictionary = Trie.fromFoldable
                   <<< map (\wd -> Tuple (List.fromFoldable wd) wd)
                   <<< List.fromFoldable

foreign import allowedWordStringsMinusAnswers :: Array String
foreign import allowedAnswerStrings :: Array String

allowedWords :: Array Word
allowedWords = Array.mapMaybe parseWord allowedWordStringsMinusAnswers
            <> allowedAnswers

allowedAnswers :: Array Word
allowedAnswers = Array.mapMaybe parseWord allowedAnswerStrings

defaultDictionary :: Dictionary
defaultDictionary = wordListToDictionary allowedWords

randomAnswer :: Effect Word
randomAnswer = do
    i <- randomInt 0 (Array.length allowedAnswers - 1)
    case allowedAnswers Array.!! i of
      Nothing -> throwException (error "randomAnswer error")
      Just w  -> pure w

