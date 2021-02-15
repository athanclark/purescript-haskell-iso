let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

let overrides =
      { argonaut-generic = upstream.argonaut-generic ⫽ { version = "v5.0.0" }
      , argonaut = upstream.argonaut ⫽ { version = "v6.0.0" }
      , argonaut-codecs = upstream.argonaut-codecs ⫽ { version = "v6.0.2" }
      , argonaut-traversals =
          upstream.argonaut-traversals ⫽ { version = "v6.0.0" }
      }

let additions =
      { yarn =
        { dependencies =
          [ "strings", "arrays", "generics-rep", "partial", "unicode" ]
        , repo = "git://github.com/Thimoteus/purescript-yarn.git"
        , version = "70f7aba66e52d2f4e60fff068df1e5e71129fe8a"
        }
      , zeromq =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "console"
          , "effect"
          , "foreign"
          , "generics-rep"
          , "node-buffer"
          , "nullable"
          , "prelude"
          ]
        , repo = "git://github.com/athanclark/purescript-zeromq.git"
        , version = "3987a40f0561ee144224588a5b7653667bbbc31b"
        }
      , bignumber =
        { dependencies =
          [ "prelude"
          , "either"
          , "exceptions"
          , "tuples-native"
          , "integers"
          , "functions"
          , "generics-rep"
          , "row-extra"
          ]
        , repo = "git://github.com/athanclark/purescript-bignumber.git"
        , version = "63a9ce8e03d6179555bcf402df8f6e638d15148e"
        }
      }

in  upstream ⫽ overrides ⫽ additions
