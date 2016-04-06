module LoginPage where

import Effects exposing (Effects)
import Html exposing (Html, div, text, button, input, label, form)
import Html.Attributes exposing (class, type', value)
import Html.Events exposing (onSubmit, onClick)
import Routes
import Signal
import Task exposing (Task)
import Util exposing (onInput)
import Http exposing (defaultSettings)
import Json.Encode


type alias Model =
  { email : String
  , password : String
  , error : Maybe String
  }


init : Model
init = Model "" "" Nothing


type Action
    = NoOp
    | SetEmail String
    | SetPassword String
    | AttemptLogin
    | ShowError Http.RawError
    | HandleResponse (Maybe Http.Response)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    SetEmail email ->
      ({ model | email = email }, Effects.none)
    SetPassword password ->
      ({ model | password = password }, Effects.none)
    ShowError error ->
      (model, Effects.none)
    AttemptLogin ->
      (model, postLogin model)
    HandleResponse response ->
      case response of
        Nothing -> 
          (model, Effects.none)
        Just response ->
          if response.status == 200 then
            ( model
            , Effects.map (\_ -> NoOp) (Routes.redirect Routes.Home)
            )
          else
            ( { model | error = Just "Something went wrong." }
            , Effects.none
            )

view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "panel panel-default" ]
    [ div [ class "panel-heading" ] [ text "Please login" ]
    , div [ class "panel-body" ]
        [ form [ onSubmit address AttemptLogin ]
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
        , text model.email
        , text model.password
    ]

postLogin : Model -> Effects Action
postLogin model =
  let
    json = Json.Encode.object
      [ ("email", Json.Encode.string model.email)
      , ("password", Json.Encode.string model.password)
      ]    
    body = Json.Encode.encode 0 json |> Http.string
  in
    postJson "/api/v1/login" body
      |> Task.toMaybe
      |> Task.map HandleResponse
      |> Effects.task

postJson : String -> Http.Body -> Task Http.RawError Http.Response
postJson url body =
  Http.send { defaultSettings | withCredentials = True }
    { verb = "POST"
    , headers = [("Content-type", "application/json")]
    , url = url
    , body = body
    }
