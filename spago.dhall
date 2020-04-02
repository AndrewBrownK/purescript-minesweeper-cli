{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff"
    , "console"
    , "effect"
    , "generics-rep"
    , "integers"
    , "maybe"
    , "node-readline"
    , "node-readline-aff"
    , "psci-support"
    , "random"
    , "refs"
    , "stringutils"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
