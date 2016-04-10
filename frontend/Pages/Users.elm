module Pages.Users where

import Effects exposing (Effects)
import Html exposing (Html, div, text, table, thead, tbody, th, tr, td, a)
import Html.Attributes exposing (class)
import Routes exposing (clickTo, encode)
import Signal exposing (Address)
import Task exposing (Task)
import Util exposing (nothing)
import Json.Decode exposing ((:=))
import Bootstrap.Alert
import Http.Extra exposing (..)

-- MODEL

type alias Model =
  { users : List User
  , error : Maybe String
  }

type alias User =
  { id : Int
  , firstName : String
  , lastName : String
  , mealCount : Int
  }

init : Model
init = Model [] Nothing

-- UPDATE

type Action
    = NoOp
    | HandleUsersResponse (Result (Error String) (Response (List User)))
    | ClearError

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    HandleUsersResponse result ->
      case result of
        Ok response ->
          ({ model | users = response.data }, Effects.none)
        Err error ->
          handleError error model
    ClearError ->
      ({ model | error = Nothing}, Effects.none)

handleError : Error String -> Model -> (Model, Effects Action)
handleError error model =
  case error of
    BadResponse response ->
      if response.status == 401 then
        (model, Effects.map (always NoOp) (Routes.redirect Routes.Login))
      else
        ({ model | error = Just response.data}, Effects.none)
    _ ->
      ({ model | error = Just "Something went wrong"}, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ errorAlert address model.error
    , table [ class "table table-striped" ]
      [ thead []
          [ tr []
              [ td [] [ text "Id" ]
              , td [] [ text "First Name" ]
              , td [] [ text "Last Name" ]
              , td [] [ text "# of Recorded Meals" ]
              ]
          ]
      , tbody [] (List.map renderRow model.users)
      ]
    ]

renderRow : User -> Html
renderRow {id,firstName,lastName,mealCount} =
  tr []
    [ td []
        [ a (clickTo <| Routes.encode (Routes.User id)) [ (toString >> text) id ]
        ]
    , td [] [ text firstName ]
    , td [] [ text lastName ]
    , td [] [ (toString >> text) mealCount ]
    ]

errorAlert : Address Action -> Maybe String -> Html
errorAlert address error =
  case error of
    Just message ->
      Bootstrap.Alert.dismissible address ClearError "danger" message
    Nothing ->
      nothing

-- TASKS AND HELPERS

fetchUsers : Effects Action
fetchUsers =
  get "/api/v1/users"
    |> withHeader "Content-Type" "application/json"
    |> withCredentials
    |> send (jsonReader usersDecoder) (jsonReader errorDecoder)
    |> Task.toResult
    |> Task.map HandleUsersResponse
    |> Effects.task

usersDecoder : Json.Decode.Decoder (List User)
usersDecoder =
  Json.Decode.list userDecoder

userDecoder : Json.Decode.Decoder User
userDecoder =
  Json.Decode.object4
    User
    ("id" := Json.Decode.int)
    ("first_name" := Json.Decode.string)
    ("last_name" := Json.Decode.string)
    ("meal_count" := Json.Decode.int)

displayError : Error String -> String
displayError error =
  case error of
    BadResponse response -> response.data
    _ -> "Something went wrong"

successDecoder : Json.Decode.Decoder Int
successDecoder =
  Json.Decode.object1
    identity
    ("user_id" := Json.Decode.int)

errorDecoder : Json.Decode.Decoder String
errorDecoder =
  ("error" := Json.Decode.string)