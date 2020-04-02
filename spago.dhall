{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-sample-minesweeper-cli"
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
    , "refs"
    , "stringutils"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
