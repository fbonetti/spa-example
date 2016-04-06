module Bootstrap.Form where

import Html exposing (Html, div, label, input, span, text)
import Html.Attributes exposing (class, type', value)
import Util exposing (onInput)
import Signal exposing (Address)
import String

textInput : Address a -> (String -> a) -> String -> String -> String -> Html
textInput = genericInput "text"

passwordInput : Address a -> (String -> a) -> String -> String -> String -> Html
passwordInput = genericInput "password"

genericInput : String -> Address a -> (String -> a) -> String -> String -> String -> Html
genericInput inputType address action inputLabel inputValue error =
  div [ class ("form-group " ++ (if String.isEmpty error then "" else "has-error")) ]
    [ label [ class "control-label" ] [ text inputLabel ]
    , input [ type' inputType, class "form-control", onInput address action, value inputValue ] []
    , span [ class "help-block" ] [ text error ]
    ]