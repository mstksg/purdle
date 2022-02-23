{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purdle"
, dependencies =
  [ "console"
  , "prelude"
  , "foldable-traversable"
  , "ordered-collections"
  , "transformers"
  , "halogen"
  , "undefined"
  , "sequences"
  , "arrays"
  , "either"
  , "lazy"
  , "maybe"
  , "newtype"
  , "orders"
  , "tuples"
  , "lists"
  , "effect"
  , "exceptions"
  , "web-dom"
  , "bifunctors"
  , "strings"
  , "random"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
