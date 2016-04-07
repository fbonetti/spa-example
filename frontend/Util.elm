module Util where

import Html exposing (Html, Attribute, text)
import Html.Events exposing (on, targetValue, onWithOptions)
import Json.Decode

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

onChange : Signal.Address a -> (String -> a) -> Attribute
onChange address contentToValue =
  on "change" targetValue (\str -> Signal.message address (contentToValue str))

onSubmitPreventDefault : Signal.Address a -> a -> Attribute
onSubmitPreventDefault address action =
  onWithOptions
    "submit"
    { stopPropagation = True, preventDefault = True }
    Json.Decode.value
    (\_ -> Signal.message address action)

nothing : Html
nothing = text ""