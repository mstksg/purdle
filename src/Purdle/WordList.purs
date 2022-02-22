
module Purdle.WordList where

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Tuple
import Data.Array as Array
import Data.Either
import Data.HTTP.Method (Method(..))
import Data.Letter
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe
import Data.String as String
import Data.Traversable
import Data.Trie as Trie
import Data.V5
import Effect.Aff
import Prelude
import Purdle.Types

allowedWordsUrl :: String
allowedWordsUrl = "https://gist.githubusercontent.com/mstksg/03b6366be14c4a4904e701f656be9aff/raw/de1df631b45492e0974f7affe266ec36fed736eb/wordle-allowed-guesses.txt"

fetchWordList :: Aff (Either String (Array Word))
fetchWordList = 
    postProcess <$> AX.request (AX.defaultRequest
      { url = allowedWordsUrl
      , method = Left GET
      , responseFormat = ResponseFormat.string }
      )
  where
    postProcess = case _ of
      Left err       -> Left (AX.printError err)
      Right response ->
        let splitWords = String.split (String.Pattern "\n") response.body
        in  Right (Array.mapMaybe parseWord splitWords)

parseWord :: String -> Maybe Word
parseWord str = do
    chrWrd <- v5FromListExact $ String.toCodePointArray (String.trim str)
    traverse (_ `Map.lookup` lookupLetterMap) chrWrd
    

wordListToDictionary :: Array Word -> Dictionary
wordListToDictionary = Trie.fromMap
                   <<< Map.fromFoldable
                   <<< map (\wd -> Tuple (List.fromFoldable wd) wd)
                   <<< List.fromFoldable

