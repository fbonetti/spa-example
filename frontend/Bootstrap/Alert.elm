module Bootstrap.Alert where

import Html exposing (Html, Attribute, div, button, span, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, type')
import Signal exposing (Address)

dismissible : Address a -> a -> String -> String -> Html
dismissible address action alertClass message =
  div [ class ("alert alert-" ++ alertClass) ]
    [ button [ type' "button", class "close", onClick address action ]
        [ span [] [ text "×" ]
        ]
    , text message
    ]

static : String -> String -> Html
static alertClass message =
  div [ class ("alert alert-" ++ alertClass) ]
    [ text message
    ]
