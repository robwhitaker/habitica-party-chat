{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "habitica-party-chat"
, dependencies =
    [ "console"
    , "effect"
    , "halogen"
    , "psci-support"
    , "web-events"
    , "web-socket"
    ]
, packages = ./packages.dhall
, sources = [ "client/src/**/*.purs", "client/test/**/*.purs" ]
}
