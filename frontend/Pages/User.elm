module Pages.User where

import Effects exposing (Effects)
import Html exposing (Html, div, table, thead, tbody, th, tr, td, text, h3, hr, h2, button, form, span, label, select, option, i, input, a)
import Html.Attributes exposing (class, type', value, selected, href)
import Html.Events exposing (onClick)
import Routes
import Signal exposing (Address)
import Task exposing (Task)
import Util exposing (onInput, nothing, onSubmitPreventDefault, onClickPreventDefault, onChange, unixToDate, safeStrToInt, dateFormat)
import Json.Encode exposing (Value)
import Json.Decode exposing ((:=))
import Bootstrap.Form
import Bootstrap.Alert
import Http.Extra exposing (..)
import Date exposing (Date)
import Date.Format
import Date.Field
import Date.Compare
import String
import Set

-- MODEL

type alias Model =
  { currentTime : Date
  , user : Maybe User
  , error : Maybe String
  , firstName : String
  , lastName : String
  , dailyLimit : Int
  , editingUser : Bool
  , description : String
  , calories : Int
  , startDate : String
  , endDate : String
  , startHour : Int
  , endHour : Int
  }

type alias User =
  { id : Int
  , firstName : String
  , lastName : String
  , dailyLimit : Int
  , meals : List Meal
  }

type alias Meal =
  { id : Int
  , description : String
  , calories : Int
  , createdAt : Int
  }

init : Date -> Model
init currentTime =
  { currentTime = currentTime
  , user = Nothing
  , error = Nothing
  , firstName = ""
  , lastName = ""
  , dailyLimit = 0
  , editingUser = False
  , description = ""
  , calories = 0
  , startDate = ""
  , endDate = ""
  , startHour = -1
  , endHour = -1
  }

newMealValid : Model -> Bool
newMealValid {description,calories} =
  String.length description > 0 && calories > 0

todaysMeals : Date -> List Meal -> List Meal
todaysMeals currentTime meals =
  let
    createdAtToIsoDateStr = ((*) 1000 >> toFloat >> Date.fromTime >> Date.Format.isoDateString)
    currentDateStr = Date.Format.isoDateString currentTime
  in
    List.filter
      (\{createdAt} -> createdAtToIsoDateStr createdAt == currentDateStr)
      meals

todaysCalories : Date -> List Meal -> Int
todaysCalories currentTime =
  todaysMeals currentTime >> List.map .calories >> List.sum

anyFilters : Model -> Bool
anyFilters {startDate,endDate,startHour,endHour} =
  String.length startDate > 0 ||
  String.length endDate > 0 ||
  startHour > -1 ||
  endHour > -1

-- UPDATE

type Action
    = NoOp
    | SetFirstName String
    | SetLastName String
    | SetDailyLimit Int
    | SetDescription String
    | SetEditingUser Bool
    | SetCalories Int
    | SetStartDate String
    | SetEndDate String
    | SetStartHour Int
    | SetEndHour Int
    | ClearFilters
    | SubmitAddMeal
    | HandleUserResponse (Result (Error String) (Response User))
    | HandleAddMealResponse (Result (Error String) (Response Meal))
    | SubmitUserEdit
    | HandleEditUserResponse (Result (Error String) (Response User))
    | DeleteMeal Int
    | HandleDeleteMealResponse (Result (Error String) (Response Int))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    SetFirstName firstName ->
      ({ model | firstName = firstName }, Effects.none)
    SetLastName lastName ->
      ({ model | lastName = lastName }, Effects.none)
    SetDailyLimit dailyLimit ->
      ({ model | dailyLimit = dailyLimit }, Effects.none)
    SetEditingUser editingUser ->
      ({ model | editingUser = editingUser }, Effects.none)
    SetDescription description ->
      ({ model | description = description }, Effects.none)
    SetCalories calories ->
      ({ model | calories = calories }, Effects.none)
    SetStartDate startDate ->
      ({ model | startDate = startDate }, Effects.none)
    SetEndDate endDate ->
      ({ model | endDate = endDate }, Effects.none)
    SetStartHour startHour ->
      ({ model | startHour = startHour }, Effects.none)
    SetEndHour endHour ->
      ({ model | endHour = endHour }, Effects.none)
    ClearFilters ->
      ({ model | startDate = "", endDate = "", startHour = -1, endHour = -1 }
      , Effects.none)
    SubmitAddMeal ->
      if newMealValid model then
        (model, postMeal model)
      else
        (model, Effects.none)
    SubmitUserEdit ->
      (model, postUserEdit model)
    HandleUserResponse result ->
      case result of
        Ok response ->
          ({ model | user = Just response.data,
              firstName = response.data.firstName,
              lastName = response.data.lastName,
              dailyLimit = response.data.dailyLimit,
              error = Nothing }
          , Effects.none)
        Err error ->
          handleError error model
    HandleAddMealResponse result ->
      case result of
        Ok response ->
          ({ model |
              user = appendMeal response.data model.user,
              description = "",
              calories = 0,
              error = Nothing }
          , Effects.none
          )
        Err error ->
          handleError error model
    HandleEditUserResponse result ->
      case result of
        Ok response ->
          ({ model |
              user = Just response.data,
              firstName = response.data.firstName,
              lastName = response.data.lastName,
              dailyLimit = response.data.dailyLimit,
              editingUser = False,
              error = Nothing }
          , Effects.none
          )
        Err error ->
          handleError error model
    DeleteMeal mealId ->
      (model, deleteMeal mealId)
    HandleDeleteMealResponse result ->
      case result of
        Ok response ->
          ({ model | user = removeMeal response.data model.user }
          , Effects.none
          )
        Err error ->
          handleError error model

appendMeal : Meal -> Maybe User -> Maybe User
appendMeal meal user =
  case user of
    Just u ->
      Just { u | meals = meal :: u.meals }
    Nothing -> Nothing

removeMeal : Int -> Maybe User -> Maybe User
removeMeal mealId user =
  case user of
    Just u ->
      Just { u | meals = List.filter (\meal -> meal.id /= mealId) u.meals }
    Nothing -> Nothing

handleError : Error String -> Model -> (Model, Effects Action)
handleError error model =
  case error of
    BadResponse response ->
      if response.status == 401 then
        (model, Effects.map (always NoOp) (Routes.redirect Routes.Login))
      else
        ({ model | error = Just response.data }, Effects.none)
    _ ->
      ({ model | error = Just "Something went wrong"}, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.user of
    Just user ->
      renderPage address model user
    Nothing ->
      case model.error of
        Just message -> renderError message
        Nothing -> text "Loading..."

renderError : String -> Html
renderError errorMessage =
  Bootstrap.Alert.static "danger" errorMessage

renderPage : Address Action -> Model -> User -> Html
renderPage address model user =
  div []
    [ case model.error of
        Just errorMessage -> renderError errorMessage
        Nothing -> nothing
    , (if model.editingUser then userStatsForm address model else userStats address model user)
    , addMeal address model user
    , hr [] []
    , mealsTable address model user.meals
    ]

userStats : Address Action -> Model -> User -> Html
userStats address {currentTime} user =
  let
    caloriesToday = todaysCalories currentTime user.meals
  in
    div [ onClick address (SetEditingUser True) ]
      [ h2 []
          [ i [ class "fa fa-pencil" ] []
          , text " "
          , text (user.firstName ++ " " ++ user.lastName ++ "'s Meals")
          ]
      , h3 [] [ text ("Daily Limit: " ++ (toString user.dailyLimit)) ]
      , h3 [ class (if caloriesToday > user.dailyLimit then "text-danger" else "text-success")]
          [ text ("Today's calories: " ++ (toString caloriesToday)) ]
      ]

userStatsForm : Address Action -> Model -> Html
userStatsForm address {firstName,lastName,dailyLimit} =
  form [ onSubmitPreventDefault address SubmitUserEdit ]
    [ input [ type' "text", value firstName, onChange address SetFirstName ] []
    , input [ type' "text", value lastName, onChange address SetLastName ] []
    , input [ type' "text", value (toString dailyLimit), onChange address (safeStrToInt >> SetDailyLimit) ] []
    , button [ type' "submit" ] [ text "save" ]
    ]

addMeal : Address Action -> Model -> User -> Html
addMeal address model user =
  let
    btnClass =
      if newMealValid model then
        "btn btn-primary"
      else
        "btn btn-primary disabled"

  in
    div []
      [ h3 [] [ text "Record Meal" ]
      , form [ onSubmitPreventDefault address SubmitAddMeal ]
          [ Bootstrap.Form.textInput address SetDescription "Description" model.description ""
          , Bootstrap.Form.numberInput address (safeStrToInt >> SetCalories) "Calories" (if model.calories > 0 then toString model.calories else "") ""
          , button [ type' "submit", class btnClass ] [ text "Record meal" ]
          ]
      ]

filterByStartDate : Model -> List Meal -> List Meal
filterByStartDate model meals =
  if model.startDate == "" then
    meals
  else
    let
      mealDate = .createdAt >> unixToDate
      startHour = if model.startHour == -1 then 0 else model.startHour
      startMinute = 0
      startDate =
        Date.fromString model.startDate
          |> Result.toMaybe
          |> Maybe.withDefault (Date.fromTime 0)
          |> Date.Field.fieldToDateClamp (Date.Field.Hour startHour)
          |> Date.Field.fieldToDateClamp (Date.Field.Minute startMinute)
      predicate meal =
        Date.Compare.is Date.Compare.SameOrAfter (mealDate meal) startDate
    in
      List.filter predicate meals

filterByEndDate : Model -> List Meal -> List Meal
filterByEndDate model meals =
  if model.endDate == "" then
    meals
  else
    let
      mealDate = .createdAt >> unixToDate
      endHour = if model.endHour == -1 then 23 else model.endHour
      endMinute = if model.endHour == -1 then 59 else 0
      endDate =
        Date.fromString model.endDate
          |> Result.toMaybe
          |> Maybe.withDefault (Date.fromTime 0)
          |> Date.Field.fieldToDateClamp (Date.Field.Hour endHour)
          |> Date.Field.fieldToDateClamp (Date.Field.Minute endMinute)
      predicate meal =
        Date.Compare.is Date.Compare.SameOrBefore (mealDate meal) endDate
    in
      List.filter predicate meals

filterByStartHour : Model -> List Meal -> List Meal
filterByStartHour {startHour} =
  List.filter (\meal -> startHour == -1 || ((unixToDate >> Date.hour) meal.createdAt) >= startHour)

filterByEndHour : Model -> List Meal -> List Meal
filterByEndHour {endHour} =
  List.filter (\meal -> endHour == -1 || ((unixToDate >> Date.hour) meal.createdAt) < endHour)

filterMeals : Model -> List Meal -> List Meal
filterMeals model meals =
  meals
    |> filterByStartDate model
    |> filterByEndDate model
    |> filterByStartHour model
    |> filterByEndHour model


mealsTable : Address Action -> Model -> List Meal -> Html
mealsTable address model meals =
  div []
    [ h3 []
        [ text "All Meals"
        , if anyFilters model then
            span [ class "btn btn-link", onClick address ClearFilters ]
              [ text "clear filters" ]
          else
            nothing
        ]
    , mealsFilters address model meals
    , table [ class "table table-striped" ]
        [ thead []
            [ tr [] 
              [ th [] [ text "Recorded At" ]
              , th [] [ text "Description" ]
              , th [] [ text "Calories" ]
              , th [] [ text "" ]
              ]
            ]
        , tbody [] (List.map (renderRow address) (filterMeals model meals))
        ]
    ]

mealsFilters : Address Action -> Model -> List Meal -> Html
mealsFilters address model meals =
  div [ class "row" ]
    [ div [ class "col-sm-3" ]
        [ div [ class "form-group" ]
          [ label [ class "control-label" ] [ text "Start Date" ]
          , select
              [ class "form-control", onChange address SetStartDate ]
              (mealFiltersDateOptions model.startDate meals)
          ]
        ]
    , div [ class "col-sm-3" ]
        [ div [ class "form-group" ]
          [ label [ class "control-label" ] [ text "End Date" ]
          , select
              [ class "form-control", onChange address SetEndDate ]
              (mealFiltersDateOptions model.endDate meals)
          ]
        ]
    , div [ class "col-sm-3" ]
        [ div [ class "form-group" ]
          [ label [ class "control-label" ] [ text "Start Time" ]
          , select
              [ class "form-control", onChange address (safeStrToInt >> SetStartHour) ]
              (mealFiltersTimeOptions model.startHour)
          ]
        ]
    , div [ class "col-sm-3" ]
        [ div [ class "form-group" ]
          [ label [ class "control-label" ] [ text "End Time" ]
          , select
              [ class "form-control", onChange address (safeStrToInt >> SetEndHour) ]
              (mealFiltersTimeOptions model.endHour)
          ]
        ]
    ]

mealFiltersDateOptions : String -> List Meal -> List Html
mealFiltersDateOptions selectedDate meals =
  let
    uniqueDates =
      List.map (.createdAt >> unixToDate >> dateFormat "%b %e, %Y") meals
        |> Set.fromList
        |> Set.toList
    toOption dateStr =
      option
        [ value dateStr, selected (selectedDate == dateStr) ]
        [ text dateStr ]
  in
    List.map toOption ("" :: uniqueDates)

mealFiltersTimeOptions : Int -> List Html
mealFiltersTimeOptions selectedVal =
  let
    indicator val =
      if val >= 12 then "PM" else "AM"
    valToString val =
      if val == -1 then
        ""
      else if (val % 12) == 0 then
        "12 " ++ (indicator val)
      else
        (toString (val % 12)) ++ " " ++ (indicator val)
    buildOption val str =
      option
        [ value (toString val), selected (val == selectedVal) ]
        [ text str ]
  in
    List.map2 buildOption [-1..23] (List.map valToString [-1..23])

renderRow : Address Action -> Meal -> Html
renderRow address {id,description,calories,createdAt} =
  tr []
    [ td [] [ (formatTimestamp >> text) createdAt ]
    , td [] [ text description ]
    , td [] [ (toString >> text) calories ]
    , td []
        [ a [ href "#", onClickPreventDefault address (DeleteMeal id) ] [ text "Delete" ]
        ]
    ]

formatTimestamp : Int -> String
formatTimestamp =
  toFloat >> (*) 1000 >> Date.fromTime >> dateFormat "%b %e, %Y at %l:%M %P"

-- TASKS AND HELPERS

fetchUser : Int -> Effects Action
fetchUser id =
  get ("/api/v1/users/" ++ toString id)
    |> withHeader "Content-Type" "application/json"
    |> withCredentials
    |> send (jsonReader userDecoder) (jsonReader errorDecoder)
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

postUserEdit : Model -> Effects Action
postUserEdit {firstName, lastName, dailyLimit, user} =
  let
    json = Json.Encode.object
      [ ("first_name", Json.Encode.string firstName)
      , ("last_name", Json.Encode.string lastName)
      , ("daily_limit", Json.Encode.int dailyLimit)
      ]
  in
    patch ("/api/v1/users/" ++ (getUserId >> toString) user)
      |> withHeader "Content-Type" "application/json"
      |> withCredentials
      |> withJsonBody json
      |> send (jsonReader userDecoder) (jsonReader errorDecoder)
      |> Task.toResult
      |> Task.map HandleEditUserResponse
      |> Effects.task

deleteMeal : Int -> Effects Action
deleteMeal mealId =
  delete ("/api/v1/meals/" ++ (toString mealId))
    |> withHeader "Content-Type" "application/json"
    |> withCredentials
    |> send (jsonReader ("meal_id" := Json.Decode.int)) (jsonReader errorDecoder)
    |> Task.toResult
    |> Task.map HandleDeleteMealResponse
    |> Effects.task

userDecoder : Json.Decode.Decoder User
userDecoder =
  Json.Decode.object5
    User
    ("id" := Json.Decode.int)
    ("first_name" := Json.Decode.string)
    ("last_name" := Json.Decode.string)
    ("daily_limit" := Json.Decode.int)
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
  ("error" := Json.Decode.string)