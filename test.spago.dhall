let conf = ./spago.dhall
let deps =
    [ "effect"
    , "aff"
    , "spec"
    ]

in conf //
    { sources = conf.sources # [ "test/**/*.purs" ]
    , dependencies = conf.dependencies # deps
    }
