module Util where

import Html exposing (Html, Attribute)
import Html.Events exposing (on, targetValue)
import Http

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

bodyEncode : List (String,String) -> Http.Body
bodyEncode =
  List.map (\(key,value) -> Http.stringData key value) >> Http.multipart