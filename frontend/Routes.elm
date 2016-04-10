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
  | User Int
  | Users
  | EmptyRoute


routeParsers : List (Matcher Route)
routeParsers =
  [ static Home "/"
  , static Login "/login"
  , static Register "/register"
  , dyn1 User "/users/" int "" 
  , static Users "/users"
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
    User id -> "/users/" ++ toString id
    Users -> "/users"
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