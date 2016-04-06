module Routes where

import Effects exposing (Effects)
import RouteParser exposing (..)
import TransitRouter


type Route
  = Home
  | Login
  | Page Int
  | EmptyRoute


routeParsers : List (Matcher Route)
routeParsers =
  [ static Home "/"
  , static Login "/login"
  , dyn1 Page "/page/" int ""
  ]


decode : String -> Route
decode path =
  RouteParser.match routeParsers path
    |> Maybe.withDefault EmptyRoute


encode : Route -> String
encode route =
  case route of
    Home -> "/"
    Login -> "/login"
    Page i -> "/page/" ++ toString i
    EmptyRoute -> ""

redirect : Route -> Effects ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task
