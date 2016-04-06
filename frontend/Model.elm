module Model where

import TransitRouter exposing (WithRoute)
import Routes exposing (Route)
import Pages.Login
import Pages.Register


type alias Model = WithRoute Route
  { page : Int
  , loginModel : Pages.Login.Model
  , registerModel : Pages.Register.Model
  }


type Action
    = NoOp
    | LoginPageAction Pages.Login.Action
    | RegisterPageAction Pages.Register.Action
    | RouterAction (TransitRouter.Action Route)
