{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "haskell-iso"
, dependencies =
  [ "console"
  , "effect"
  , "email-validate"
  , "js-date"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "string-parsers"
  , "typelevel"
  , "unorm"
  , "uri"
  , "argonaut"
  , "node-buffer"
  , "yarn"
  , "zeromq"
  , "uuid"
  , "bignumber"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
