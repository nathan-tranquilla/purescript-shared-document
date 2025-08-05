{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "maybe"
  , "prelude"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
