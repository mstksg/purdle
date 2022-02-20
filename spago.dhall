{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "prelude"
  , "foldable-traversable"
  , "ordered-collections"
  , "transformers"
  , "halogen"
  , "undefined"
  , "sequences"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
