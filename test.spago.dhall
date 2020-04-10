let conf = ./spago.dhall
let deps =
    [ "effect"
    , "spec"
    ]

in conf //
    { sources = conf.sources # [ "test/**/*.purs" ]
    , dependencies = conf.dependencies # deps
    }
