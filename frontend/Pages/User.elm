module Pages.User where

import Effects exposing (Effects)
import Html exposing (Html, div, table, thead, tbody, th, tr, td, text, h3, hr, h2, button, form)
import Html.Attributes exposing (class, type')
import Routes
import Signal exposing (Address)
import Task exposing (Task)
import Util exposing (onInput, nothing, onSubmitPreventDefault)
import Json.Encode exposing (Value)
import Json.Decode exposing ((:=))
import Bootstrap.Form
import Http.Extra exposing (..)
import Date
import Date.Format
import String

-- MODEL

type alias Model =
  { user : Maybe User
  , error : Maybe String
  , description : String
  , calories : Int
  }

type alias User =
  { id : Int
  , firstName : String
  , lastName : String
  , meals : List Meal
  }

type alias Meal =
  { id : Int
  , description : String
  , calories : Int
  , createdAt : Int
  }

init : Model
init = Model Nothing Nothing "" 0

-- UPDATE

type Action
    = NoOp
    | SetDescription String
    | SetCalories Int
    | SubmitAddMeal
    | HandleUserResponse (Result (Error String) (Response User))
    | HandleAddMealResponse (Result (Error String) (Response Meal))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    SetDescription description ->
      ({ model | description = description }, Effects.none)
    SetCalories calories ->
      ({ model | calories = calories }, Effects.none)
    SubmitAddMeal ->
      ( model
      , postMeal model
      )
    HandleUserResponse result ->
      case result of
        Ok response ->
          ({ model | user = Just response.data}, Effects.none)
        Err error ->
          handleError error model
    HandleAddMealResponse result ->
      case result of
        Ok response ->
          ({ model | user = appendMeal model.user response.data }, Effects.none)
        Err error ->
          handleError error model

appendMeal user meal =
  case user of
    Just u ->
      Just { u | meals = meal :: u.meals }
    Nothing -> Nothing

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
      renderPage address model user
    Nothing ->
      text (model.error |> Maybe.withDefault "Something went wrong")

renderPage address model user =
  div []
    [ userStats user
    , addMeal address model user
    , hr [] []
    , mealsTable address user
    ]

userStats : User -> Html
userStats user =
  h2 [] [ text (user.firstName ++ " " ++ user.lastName ++ "'s Meals") ]

addMeal : Address Action -> Model -> User -> Html
addMeal address model user =
  div []
    [ h3 [] [ text "Record Meal" ]
    , form [ onSubmitPreventDefault address SubmitAddMeal ]
        [ Bootstrap.Form.textInput address SetDescription "Description" model.description ""
        , Bootstrap.Form.numberInput address (String.toInt >> Result.toMaybe >> Maybe.withDefault 0 >> SetCalories) "Calories" (toString model.calories) ""
        , button [ type' "submit", class "btn btn-primary" ] [ text "Record meal" ]
        ]
    ]

mealsTable : Address Action -> User -> Html
mealsTable address user =
  div []
    [ h3 [] [ text "All Meals" ]
    , table [ class "table table-striped" ]
        [ thead []
            [ tr [] 
              [ th [] [ text "Recorded At" ]
              , th [] [ text "Description" ]
              , th [] [ text "Calories" ]
              ]
            ]
        , tbody [] (List.map renderRow user.meals)
        ]
    ]

renderRow {description,calories,createdAt} =
  tr []
    [ td [] [ (formatTimestamp >> text) createdAt ]
    , td [] [ text description ]
    , td [] [ (toString >> text) calories ]
    ]

formatTimestamp : Int -> String
formatTimestamp =
  toFloat >> (*) 1000 >> Date.fromTime >> Date.Format.format "%b %e, %Y at %l:%M %P"

-- TASKS AND HELPERS

fetchUser : Int -> Effects Action
fetchUser id =
  get ("/api/v1/users/" ++ toString id)
    |> withHeader "Content-Type" "application/json"
    |> withCredentials
    |> send (jsonReader userSuccessDecoder) (jsonReader errorDecoder)
    |> Task.toResult
    |> Task.map HandleUserResponse
    |> Effects.task

getUserId : Maybe User -> Int
getUserId user =
  case user of
    Just u -> u.id
    Nothing -> 0

postMeal : Model -> Effects Action
postMeal {user, description, calories} =
  let
    json = Json.Encode.object
      [ ("user_id", Json.Encode.int (getUserId user))
      , ("description", Json.Encode.string description)
      , ("calories", Json.Encode.int calories)
      ]
  in
    post "/api/v1/meals"
      |> withHeader "Content-Type" "application/json"
      |> withCredentials
      |> withJsonBody json
      |> send (jsonReader mealDecoder) (jsonReader errorDecoder)
      |> Task.toResult
      |> Task.map HandleAddMealResponse
      |> Effects.task

userSuccessDecoder : Json.Decode.Decoder User
userSuccessDecoder = userDecoder

userDecoder : Json.Decode.Decoder User
userDecoder =
  Json.Decode.object4
    User
    ("id" := Json.Decode.int)
    ("first_name" := Json.Decode.string)
    ("last_name" := Json.Decode.string)
    ("meals" := Json.Decode.list mealDecoder)

mealDecoder : Json.Decode.Decoder Meal
mealDecoder =
  Json.Decode.object4
    Meal
    ("id" := Json.Decode.int)
    ("description" := Json.Decode.string)
    ("calories" := Json.Decode.int)
    ("created_at" := Json.Decode.int)

errorDecoder : Json.Decode.Decoder String
errorDecoder =
  Json.Decode.object1
    identity
    ("error" := Json.Decode.string)