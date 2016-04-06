module Update where

import Effects exposing (Effects, none)
import TransitRouter

import Model exposing (..)
import Routes exposing (..)
import Pages.Login


initialModel : Model
initialModel =
  { transitRouter = TransitRouter.empty EmptyRoute
  , page = 0
  , loginModel = Pages.Login.init
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

    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model


mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route model =
  case route of

    -- in a typical SPA, you might have to trigger tasks when landing on a page,
    -- like an HTTP request to load specific data

    Home ->
      (model, Effects.none)

    Login ->
      (model, Effects.none)

    Page p ->
      ({ model | page = p }, Effects.none)

    EmptyRoute ->
      (model, Effects.none)

