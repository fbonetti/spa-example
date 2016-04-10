module Pages.Register where

import Effects exposing (Effects)
import Html exposing (Html, div, p, text, button, input, label, form, br, a, span)
import Html.Attributes exposing (class, type', value, href)
import Html.Events exposing (onSubmit, onClick)
import Routes
import Signal exposing (Address)
import Task exposing (Task)
import Util exposing (onInput, onSubmitPreventDefault, nothing)
import Json.Encode exposing (Value)
import Json.Decode exposing ((:=))
import Bootstrap.Alert
import Bootstrap.Form
import Http.Extra exposing (..)
import String
import Regex exposing (regex)


-- MODEL

type alias Model =
  { firstName : String
  , lastName : String
  , email : String
  , password : String
  , passwordConfirmation : String
  , accountType : String
  , error : Maybe String
  , validate : Bool
  }


init : Model
init = Model "" "" "" "" "" "" Nothing False

formValid : Model -> Bool
formValid model =
  List.all (\f -> f model)
    [ firstNameValid
    , lastNameValid
    , emailValid
    , passwordValid
    , passwordConfirmationValid
    , accountTypeValid
    ]

nonEmptyString = String.length >> flip (>) 0


firstNameValid {firstName} = nonEmptyString firstName
lastNameValid {lastName} = nonEmptyString lastName
emailValid {email} = Regex.contains (regex ".+\\@.+\\..+") email
passwordValid {password} = String.length password >= 8
passwordConfirmationValid {password, passwordConfirmation} =
  String.length passwordConfirmation > 0 && passwordConfirmation == password
accountTypeValid {accountType} = nonEmptyString accountType

firstNameErrorMessage model =
  if not model.validate || firstNameValid model then
    ""
  else
    "First name required"

lastNameErrorMessage model =
  if not model.validate || lastNameValid model then
    ""
  else
    "Last name required"

emailErrorMessage model =
  if not model.validate || emailValid model then
    ""
  else
    "Valid email address required"

passwordErrorMessage model =
  if not model.validate || passwordValid model then
    ""
  else
    "Password must be at least 8 characters"

passwordConfirmationErrorMessage model =
  if not model.validate || passwordConfirmationValid model then
    ""
  else
    "Password confirmation must match password"

accountTypeErrorMessage model =
  if not model.validate || accountTypeValid model then
    ""
  else
    "Account type required"

-- UPDATE

type Action
    = NoOp
    | SetFirstName String
    | SetLastName String
    | SetEmail String
    | SetPassword String
    | SetPasswordConfirmation String
    | SetAccountType String
    | AttemptRegister
    | HandleRegisterResponse (Result (Error String) (Response Int))
    | ClearError

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    SetFirstName firstName ->
      ({ model | firstName = firstName }, Effects.none)
    SetLastName lastName ->
      ({ model | lastName = lastName }, Effects.none)
    SetEmail email ->
      ({ model | email = email }, Effects.none)
    SetPassword password ->
      ({ model | password = password }, Effects.none)
    SetPasswordConfirmation passwordConfirmation ->
      ({ model | passwordConfirmation = passwordConfirmation }, Effects.none)
    SetAccountType accountType ->
      ({ model | accountType = accountType }, Effects.none)
    AttemptRegister ->
      if formValid model then
        (model, postRegister model)
      else
        ({ model | validate = True}, Effects.none)
    HandleRegisterResponse result ->
      case result of
        Ok response -> 
          ( init
          , Effects.map (always NoOp) (Routes.redirect (Routes.User response.data))
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
  div [ class "row" ]
    [ div [ class "col-md-6 col-md-offset-3" ]
      [ div [ class "panel panel-default" ]
          [ div [ class "panel-heading" ] [ text "New account" ]
          , div [ class "panel-body" ]
              [ errorAlert address model.error
              , form [ onSubmitPreventDefault address AttemptRegister ]
                [ Bootstrap.Form.textInput address SetFirstName "First Name" model.firstName (firstNameErrorMessage model)
                , Bootstrap.Form.textInput address SetLastName "Last Name" model.lastName (lastNameErrorMessage model)
                , Bootstrap.Form.textInput address SetEmail "Email" model.email (emailErrorMessage model)
                , Bootstrap.Form.passwordInput address SetPassword "Password" model.password (passwordErrorMessage model)
                , Bootstrap.Form.passwordInput address SetPasswordConfirmation "Password Confirmation" model.passwordConfirmation (passwordConfirmationErrorMessage model)
                , Bootstrap.Form.selectInput address SetAccountType accountTypeOptions "Account Type" model.accountType (accountTypeErrorMessage model)
                , input [ submitBtnClass model, type' "submit", value "Login" ] []
                ]
              , br [] []
              , p [ class "text-center" ]
                  [ text "Already have an account? "
                  , a (Routes.clickTo (Routes.encode Routes.Login))
                      [ text "Click here to login" ]
                  ]
              ]
          ]
      ]
    ]

accountTypeOptions : List (String,String)
accountTypeOptions =
  [ ("Regular User", "RegularUser")
  , ("User Manager", "UserManager")
  , ("Admin", "Admin")
  ]

submitBtnClass model =
  if not model.validate || formValid model then
    class "btn btn-primary btn-block"
  else
    class ("btn btn-primary btn-block " ++ (if formValid model then "" else "disabled"))

errorAlert : Address Action -> Maybe String -> Html
errorAlert address error =
  case error of
    Just message ->
      Bootstrap.Alert.dismissible address ClearError "danger" message
    Nothing ->
      nothing

-- TASKS AND HELPERS

postRegister : Model -> Effects Action
postRegister {firstName,lastName,email,accountType,password,passwordConfirmation} =
  let
    json = Json.Encode.object
      [ ("first_name", Json.Encode.string firstName)
      , ("last_name", Json.Encode.string lastName)
      , ("email", Json.Encode.string email)
      , ("type", Json.Encode.string accountType)
      , ("password", Json.Encode.string password)
      , ("password_confirmation", Json.Encode.string passwordConfirmation)
      ]
  in
    post "/api/v1/register"
      |> withJsonBody json
      |> withHeader "Content-Type" "application/json"
      |> withCredentials
      |> send (jsonReader successDecoder) (jsonReader errorDecoder)
      |> Task.toResult
      |> Task.map HandleRegisterResponse
      |> Effects.task

displayError : Error String -> String
displayError error =
  case error of
    BadResponse response -> response.data
    _ -> "Something went wrong"

successDecoder : Json.Decode.Decoder Int
successDecoder =
  ("user_id" := Json.Decode.int)

errorDecoder : Json.Decode.Decoder String
errorDecoder =
  ("error" := Json.Decode.string)