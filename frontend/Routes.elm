module Routes where

import Effects exposing (Effects)
import RouteParser exposing (..)
import TransitRouter

import Html
import Html.Attributes
import Html.Events
import Json.Decode


type Route
  = Home
  | Login
  | Register
  | Page Int
  | EmptyRoute


routeParsers : List (Matcher Route)
routeParsers =
  [ static Home "/"
  , static Login "/login"
  , static Register "/register"
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
    Register -> "/register"
    Page i -> "/page/" ++ toString i
    EmptyRoute -> ""

redirect : Route -> Effects ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task

clickTo : String -> List Html.Attribute
clickTo path =
  [ Html.Attributes.href path
  , Html.Events.onWithOptions
      "click"
      { stopPropagation = True, preventDefault = True }
      Json.Decode.value
      (\_ -> Signal.message TransitRouter.pushPathAddress path)
  ]