module Util where

import Html exposing (Html, Attribute, text)
import Html.Events exposing (on, targetValue)
import Http

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

onChange : Signal.Address a -> (String -> a) -> Attribute
onChange address contentToValue =
  on "change" targetValue (\str -> Signal.message address (contentToValue str))

nothing : Html
nothing = text ""