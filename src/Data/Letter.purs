
module Data.Letter where

import Data.Foldable
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Tuple
import Prelude

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M
            | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

derive instance Eq Letter
derive instance Ord Letter

instance Show Letter where
    show = String.singleton <<< String.codePointFromChar <<< letterChar

allLetters :: Array Letter
allLetters =
  [ A , B , C , D , E , F , G , H , I , J , K , L , M
  , N , O , P , Q , R , S , T , U , V , W , X , Y , Z
  ]

letterChar :: Letter -> Char
letterChar = case _ of
    A -> 'A'
    B -> 'B'
    C -> 'C'
    D -> 'D'
    E -> 'E'
    F -> 'F'
    G -> 'G'
    H -> 'H'
    I -> 'I'
    J -> 'J'
    K -> 'K'
    L -> 'L'
    M -> 'M'
    N -> 'N'
    O -> 'O'
    P -> 'P'
    Q -> 'Q'
    R -> 'R'
    S -> 'S'
    T -> 'T'
    U -> 'U'
    V -> 'V'
    W -> 'W'
    X -> 'X'
    Y -> 'Y'
    Z -> 'Z'

letterCharLower :: Letter -> Char
letterCharLower = case _ of
    A -> 'a'
    B -> 'b'
    C -> 'c'
    D -> 'd'
    E -> 'e'
    F -> 'f'
    G -> 'g'
    H -> 'h'
    I -> 'i'
    J -> 'j'
    K -> 'k'
    L -> 'l'
    M -> 'm'
    N -> 'n'
    O -> 'o'
    P -> 'p'
    Q -> 'q'
    R -> 'r'
    S -> 's'
    T -> 't'
    U -> 'u'
    V -> 'v'
    W -> 'w'
    X -> 'x'
    Y -> 'y'
    Z -> 'z'

lookupLetterMap :: Map String.CodePoint Letter
lookupLetterMap = Map.fromFoldable $
    flip foldMap allLetters \l ->
             Tuple (String.codePointFromChar $ letterChar l) l
      List.: Tuple (String.codePointFromChar $ letterCharLower l) l
      List.: List.nil

