
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200331/packages.dhall sha256:350af1fdc68c91251138198f03ceedc4f8ed6651ee2af8a2177f87bcd64570d4

let overrides = {=}

let additions =
  { node-readline-aff =
    { dependencies =
      [ "prelude"
      , "console"
      , "psci-support"
      , "node-readline"
      , "aff"
      , "node-streams"
      , "options"
      , "exceptions"
      , "either"
      ]
    , repo =
      "https://github.com/ChrisPenner/purescript-node-readline-aff.git"
    , version =
      "v0.3.0"
    }
  }

in  upstream // overrides // additions
