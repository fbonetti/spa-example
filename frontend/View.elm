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

import Bootstrap.Alert
import Pages.Login
import Pages.Register
import Pages.User


view : Address Action -> Model -> Html
view address model =
  div []
    [ div [ class "navbar navbar-default navbar-static-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
              [ a ([ class "navbar-brand" ] ++ clickTo (Routes.encode Login)) [ text "Example App" ]
              ]
            , div [ class "navbar-collapse collapse" ]
              [ ul [ class "nav navbar-nav" ]
                  [ li [ headerLinkClass model Login ]
                      [ a (clickTo <| Routes.encode Login) [ text "Login" ] ]
                  , li [ headerLinkClass model Register ]
                      [ a (clickTo <| Routes.encode Register) [ text "Register" ] ]
                  ] 
              ]
            ]
        ]
    , div [ class "container" ]
      [ div
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
                Bootstrap.Alert.static "danger" "404: Page Not Found"
          ]
      ]
    ]

headerLinkClass : Model -> Route -> Attribute
headerLinkClass model route =
  if TransitRouter.getRoute model == route then
    class "active"
  else
    class ""
