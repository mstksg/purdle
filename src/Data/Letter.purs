
module Data.Letter where

import Prelude

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M
            | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

derive instance Eq Letter
derive instance Ord Letter

instance Show Letter where
    show = case _ of
      A -> "A"
      B -> "B"
      C -> "C"
      D -> "D"
      E -> "E"
      F -> "F"
      G -> "G"
      H -> "H"
      I -> "I"
      J -> "J"
      K -> "K"
      L -> "L"
      M -> "M"
      N -> "N"
      O -> "O"
      P -> "P"
      Q -> "Q"
      R -> "R"
      S -> "S"
      T -> "T"
      U -> "U"
      V -> "V"
      W -> "W"
      X -> "X"
      Y -> "Y"
      Z -> "Z"
