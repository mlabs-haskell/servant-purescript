let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211109/packages.dhall sha256:e8d8d5b339f6d46d950da90037c6c38e8809f7e34f727373089ab82c080fc709

let additions =
      { servant-support =
        { dependencies =
          [ "aff"
          , "affjax"
          , "argonaut"
          , "arrays"
          , "bifunctors"
          , "either"
          , "http-methods"
          , "maybe"
          , "newtype"
          , "nonempty"
          , "prelude"
          , "psci-support"
          , "strings"
          , "transformers"
          , "tuples"
          , "uri"
          ]
        , repo = "https://github.com/input-output-hk/purescript-servant-support"
        , version = "010bc7b3ea8e2707a00c23fd510201aabd4eb13f"
        }
      , json-helpers =
        { dependencies =
          [ "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "contravariant"
          , "control"
          , "either"
          , "enums"
          , "foreign-object"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "record"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
        , version = "68265aaacc1a56c00a7625d424ff13d619681e5e"
        }
      }

in  upstream // additions
