{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "cardano-transaction-lib"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "aff-retry"
  , "affjax"
  , "ansi"
  , "argonaut"
  , "argonaut-codecs"
  , "arraybuffer-types"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "bignumber"
  , "bytearrays"
  , "cardano-hd-wallet"
  , "cardano-message-signing"
  , "cardano-types"
  , "checked-exceptions"
  , "cip30"
  , "cip30-typesafe"
  , "console"
  , "control"
  , "crypto"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "encoding"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "formatters"
  , "functions"
  , "gen"
  , "heterogeneous"
  , "http-methods"
  , "identity"
  , "integers"
  , "js-bigints"
  , "js-date"
  , "lattice"
  , "lists"
  , "maybe"
  , "media-types"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "noble-secp256k1"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "node-readline"
  , "node-streams"
  , "nonempty"
  , "now"
  , "nullable"
  , "numbers"
  , "optparse"
  , "ordered-collections"
  , "orders"
  , "parallel"
  , "partial"
  , "plutus-types"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "quickcheck"
  , "quickcheck-combinators"
  , "quickcheck-laws"
  , "random"
  , "rationals"
  , "record"
  , "refs"
  , "safe-coerce"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "stringutils"
  , "tailrec"
  , "these"
  , "toppokki"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "uint"
  , "unfoldable"
  , "untagged-union"
  , "variant"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
