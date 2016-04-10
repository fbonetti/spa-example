module Update where

import Effects exposing (Effects, none)
import TransitRouter

import Model exposing (..)
import Routes exposing (..)
import Pages.Login
import Pages.Register
import Pages.User
import Pages.Users


initialModel : Model
initialModel =
  { transitRouter = TransitRouter.empty EmptyRoute
  , page = 0
  , loginModel = Pages.Login.init
  , registerModel = Pages.Register.init
  , userModel = Pages.User.init
  , usersModel = Pages.Users.init
  }


actions : Signal Action
actions =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.map RouterAction TransitRouter.actions


routerConfig : TransitRouter.Config Route Action Model
routerConfig =
  { mountRoute = mountRoute
  , getDurations = \_ _ _ -> (50, 200)
  , actionWrapper = RouterAction
  , routeDecoder = Routes.decode
  }


init : String -> (Model, Effects Action)
init path =
  TransitRouter.init routerConfig path initialModel


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    NoOp ->
      (model, Effects.none)

    LoginPageAction taskAction ->
      let (model', effects) = Pages.Login.update taskAction model.loginModel
      in ( { model | loginModel = model' }
         , Effects.map LoginPageAction effects )

    RegisterPageAction taskAction ->
      let (model', effects) = Pages.Register.update taskAction model.registerModel
      in ( { model | registerModel = model' }
         , Effects.map RegisterPageAction effects )

    UserPageAction taskAction ->
      let (model', effects) = Pages.User.update taskAction model.userModel
      in ( { model | userModel = model' }
         , Effects.map UserPageAction effects )

    UsersPageAction taskAction ->
      let (model', effects) = Pages.Users.update taskAction model.usersModel
      in ( { model | usersModel = model' }
         , Effects.map UsersPageAction effects )

    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model


-- Trigger an update or an effect when a route has mounted

mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route model =
  case route of

    Home ->
      (model, Effects.none)

    Login ->
      (model, Effects.none)

    Register ->
      (model, Effects.none)

    User id ->
      ({ model | userModel = Pages.User.init }, Effects.map UserPageAction (Pages.User.fetchUser id))

    Users ->
      (model, Effects.map UsersPageAction Pages.Users.fetchUsers)

    EmptyRoute ->
      (model, Effects.none)

