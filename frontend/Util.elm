module Util where

import Html exposing (Html, Attribute, text)
import Html.Events exposing (on, targetValue, onWithOptions)
import Json.Decode
import Dict
import Date exposing (Date)
import String
import Date.Config.Configs
import Date.Format

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

onChange : Signal.Address a -> (String -> a) -> Attribute
onChange address contentToValue =
  on "change" targetValue (\str -> Signal.message address (contentToValue str))

onClickPreventDefault : Signal.Address a -> a -> Attribute
onClickPreventDefault =
  onPreventDefault "click"

onSubmitPreventDefault : Signal.Address a -> a -> Attribute
onSubmitPreventDefault =
  onPreventDefault "submit"

onPreventDefault : String -> Signal.Address a -> a -> Attribute
onPreventDefault event address action =
  onWithOptions
    event
    { stopPropagation = True, preventDefault = True }
    Json.Decode.value
    (\_ -> Signal.message address action)

nothing : Html
nothing = text ""

safeStrToInt : String -> Int
safeStrToInt val =
  String.toInt val
    |> Result.toMaybe
    |> Maybe.withDefault 0

unixToDate : Int -> Date
unixToDate =
  (*) 1000 >> toFloat >> Date.fromTime

dateFormat : String -> Date -> String
dateFormat =
  Date.Format.format (Date.Config.Configs.getConfig "en_us")