module Pages.User where

import Effects exposing (Effects)
import Html exposing (Html, div, table, thead, tbody, th, tr, td, text)
import Html.Attributes exposing (class)
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
  { user : Maybe User
  , error : Maybe String
  }

type alias User =
  { firstName : String
  , lastName : String
  , meals : List Meal
  }

type alias Meal =
  { description : String
  , calories : Int
  , createdAt : Int
  }

init : Model
init = Model Nothing Nothing

-- UPDATE

type Action
    = NoOp
    | HandleUserResponse (Result (Error String) (Response User))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    HandleUserResponse result ->
      case result of
        Ok response ->
          ({ model | user = Just response.data}, Effects.none)
        Err error ->
          handleError error model

handleError error model =
  case error of
    BadResponse response ->
      if response.status == 401 then
        (model, Effects.map (always NoOp) (Routes.redirect Routes.Login))
      else if response.status == 404 || response.status == 403 then
        ({ model | error = Just response.data }, Effects.none)
      else
        ({ model | error = Just "Something went wrong"}, Effects.none)
    _ ->
      ({ model | error = Just "Something went wrong"}, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.user of
    Just user ->
      mealsTable address user
    Nothing ->
      text (model.error |> Maybe.withDefault "Something went wrong")

mealsTable : Signal.Address Action -> User -> Html
mealsTable address user =
  table [ class "table table-striped" ]
    [ thead []
        [ th [] [ text "Description" ]
        , th [] [ text "Calories" ]
        , th [] [ text "Timestamp" ]
        ]
    , tbody [] (List.map renderRow user.meals)
    ]

renderRow {description,calories,createdAt} =
  tr []
    [ td [] [ text description ]
    , td [] [ (toString >> text) calories ]
    , td [] [ (toString >> text) createdAt ]
    ]

-- TASKS AND HELPERS

fetchUser : Int -> Effects Action
fetchUser id =
  get ("/api/v1/users/" ++ toString id)
    |> withHeader "Content-Type" "application/json"
    |> withCredentials
    |> send (jsonReader successDecoder) (jsonReader errorDecoder)
    |> Task.toResult
    |> Task.map HandleUserResponse
    |> Effects.task

successDecoder : Json.Decode.Decoder User
successDecoder = userDecoder

userDecoder : Json.Decode.Decoder User
userDecoder =
  Json.Decode.object3
    User
    ("first_name" := Json.Decode.string)
    ("last_name" := Json.Decode.string)
    ("meals" := Json.Decode.list mealDecoder)

mealDecoder : Json.Decode.Decoder Meal
mealDecoder =
  Json.Decode.object3
    Meal
    ("description" := Json.Decode.string)
    ("calories" := Json.Decode.int)
    ("created_at" := Json.Decode.int)

errorDecoder : Json.Decode.Decoder String
errorDecoder =
  Json.Decode.object1
    identity
    ("error" := Json.Decode.string)