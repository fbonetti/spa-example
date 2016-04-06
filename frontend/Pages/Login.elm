module Pages.Login where

import Effects exposing (Effects)
import Html exposing (Html, div, text, button, input, label, form)
import Html.Attributes exposing (class, type', value)
import Html.Events exposing (onSubmit, onClick)
import Routes
import Signal exposing (Address)
import Task exposing (Task)
import Util exposing (onInput, nothing)
import Json.Encode exposing (Value)
import Json.Decode exposing ((:=))
import Bootstrap.Alert
import Http.Extra exposing (..)

-- MODEL

type alias Model =
  { email : String
  , password : String
  , error : Maybe String
  }


init : Model
init = Model "" "" Nothing

-- UPDATE

type Action
    = NoOp
    | SetEmail String
    | SetPassword String
    | AttemptLogin
    | HandleLoginResponse (Result (Error String) (Response String))
    | ClearError

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    SetEmail email ->
      ({ model | email = email }, Effects.none)
    SetPassword password ->
      ({ model | password = password }, Effects.none)
    AttemptLogin ->
      (model, postLogin model)
    HandleLoginResponse response ->
      case response of
        Ok response -> 
          ( { model | error = Nothing }
          , Effects.map (always NoOp) (Routes.redirect Routes.Home)
          )
        Err error ->
          ( { model | error = Just (displayError error) }
          , Effects.none
          )

    ClearError ->
      ({ model | error = Nothing}, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "panel panel-default" ]
    [ div [ class "panel-heading" ] [ text "Please login" ]
    , div [ class "panel-body" ]
        [ errorAlert address model.error
        , form [ onSubmit address AttemptLogin ]
          [ div [ class "form-group" ]
            [ label [ class "control-label" ] [ text "Email" ]
            , input [ type' "text", class "form-control", onInput address SetEmail ] []
            ]
          , div [ class "form-group" ]
            [ label [ class "control-label" ] [ text "Password" ]
            , input [ type' "password", class "form-control", onInput address SetPassword ] []
            ]
          ]
        , input [ class "btn btn-primary btn-block", type' "submit", value "Login", onClick address AttemptLogin ] []
        ]
    ]

errorAlert : Address Action -> Maybe String -> Html
errorAlert address error =
  case error of
    Just message ->
      Bootstrap.Alert.dismissible address ClearError "danger" message
    Nothing ->
      nothing

-- TASKS AND HELPERS

postLogin : Model -> Effects Action
postLogin model =
  let
    json = Json.Encode.object
      [ ("email", Json.Encode.string model.email)
      , ("password", Json.Encode.string model.password)
      ]
  in
    post "/api/v1/login"
      |> withJsonBody json
      |> withHeader "Content-Type" "application/json"
      |> withCredentials
      |> send (jsonReader successDecoder) (jsonReader errorDecoder)
      |> Task.toResult
      |> Task.map HandleLoginResponse
      |> Effects.task

displayError : Error String -> String
displayError error =
  case error of
    BadResponse response -> response.data
    _ -> "Something went wrong"

successDecoder : Json.Decode.Decoder String
successDecoder =
  Json.Decode.object1
    identity
    ("message" := Json.Decode.string)

errorDecoder : Json.Decode.Decoder String
errorDecoder =
  Json.Decode.object1
    identity
    ("error" := Json.Decode.string)