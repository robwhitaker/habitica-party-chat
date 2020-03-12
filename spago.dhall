{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "habitica-party-chat"
, dependencies = [ "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "client/src/**/*.purs", "client/test/**/*.purs" ]
}
