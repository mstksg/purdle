{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purdle"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "orders"
  , "prelude"
  , "random"
  , "sequences"
  , "strings"
  , "transformers"
  , "tuples"
  , "undefined"
  , "web-dom"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
