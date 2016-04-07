module Bootstrap.Form (textInput, passwordInput, numberInput, selectInput) where

import Html exposing (Html, div, label, input, span, text, select, option)
import Html.Attributes exposing (class, type', value, selected)
import Util exposing (onInput, onChange)
import Signal exposing (Address)
import String

textInput : Address a -> (String -> a) -> String -> String -> String -> Html
textInput = genericInput "text"

numberInput : Address a -> (String -> a) -> String -> String -> String -> Html
numberInput = genericInput "number"

passwordInput : Address a -> (String -> a) -> String -> String -> String -> Html
passwordInput = genericInput "password"

genericInput : String -> Address a -> (String -> a) -> String -> String -> String -> Html
genericInput inputType address action inputLabel inputValue error =
  div [ class ("form-group " ++ (if String.isEmpty error then "" else "has-error")) ]
    [ label [ class "control-label" ] [ text inputLabel ]
    , input [ type' inputType, class "form-control", onInput address action, value inputValue ] []
    , span [ class "help-block" ] [ text error ]
    ]

selectInput : Address a -> (String -> a) -> List (String,String) -> String -> String -> String -> Html
selectInput address action options inputLabel inputValue error =
  div [ class ("form-group " ++ (if String.isEmpty error then "" else "has-error")) ]
    [ label [ class "control-label" ] [ text inputLabel ]
    , select [ class "form-control", onChange address action ] (renderOptions options inputValue)
    , span [ class "help-block" ] [ text error ]
    ]  

renderOptions : List (String,String) -> String -> List Html
renderOptions options selectedVal =
  List.map
    (\(lab,val) -> option [ value val, selected (val == selectedVal) ] [ text lab ] )
    (("","") :: options)
