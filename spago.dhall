{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-minesweeper-cli"
, dependencies =
    [ "aff"
    , "console"
    , "effect"
    , "generics-rep"
    , "integers"
    , "maybe"
    , "node-readline"
    , "node-readline-aff"
    , "optparse"
    , "psci-support"
    , "random"
    , "record"
    , "refs"
    , "stringutils"
    , "validation"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
