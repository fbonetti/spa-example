module Model where

import TransitRouter exposing (WithRoute)
import Routes exposing (Route)
import Pages.Login


type alias Model = WithRoute Route
  { page : Int
  , loginModel : Pages.Login.Model
  }


type Action
    = NoOp
    | LoginPageAction Pages.Login.Action
    | RouterAction (TransitRouter.Action Route)
