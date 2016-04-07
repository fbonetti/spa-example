module Model where

import TransitRouter exposing (WithRoute)
import Routes exposing (Route)
import Pages.Login
import Pages.Register
import Pages.User


type alias Model = WithRoute Route
  { page : Int
  , loginModel : Pages.Login.Model
  , registerModel : Pages.Register.Model
  , userModel : Pages.User.Model
  }


type Action
    = NoOp
    | LoginPageAction Pages.Login.Action
    | RegisterPageAction Pages.Register.Action
    | UserPageAction Pages.User.Action
    | RouterAction (TransitRouter.Action Route)
