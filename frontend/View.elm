module View where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (..)
import Json.Decode as Json

import TransitStyle
import TransitRouter exposing (getTransition)

import Model exposing (..)
import Routes exposing (..)
import Pages.Login
import Pages.Register
import Pages.User


view : Address Action -> Model -> Html
view address model =
  div
    [ class "container" ]
    [ h1 [] [ text "Elm TransitRouter example" ]
    , div [ class "menu" ]
        [ a (clickTo <| Routes.encode Home) [ text "Home" ]
        , a (clickTo <| Routes.encode Login) [ text "Login" ]
        , a (clickTo <| Routes.encode Register) [ text "Register" ]
        , a (clickTo <| Routes.encode (Page 1)) [ text "Page 1" ]
        , a (clickTo <| Routes.encode (Page 2)) [ text "Page 2" ]
        ]
    , div
        [ class "content"
        , style (TransitStyle.fadeSlideLeft 100 (getTransition model))
        ]
        [ case (TransitRouter.getRoute model) of
            Home ->
              text <| "This is home"
            Login ->
              Pages.Login.view (Signal.forwardTo address LoginPageAction) model.loginModel
            Register ->
              Pages.Register.view (Signal.forwardTo address RegisterPageAction) model.registerModel
            User _ ->
              Pages.User.view (Signal.forwardTo address UserPageAction) model.userModel
            Page _ ->
              text <| "This is page " ++ toString model.page
            EmptyRoute ->
              text <| ""
        ]
    ]

