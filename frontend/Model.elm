module Model where

import TransitRouter exposing (WithRoute)
import Routes exposing (Route)
import TaskPage
import LoginPage


type alias Model = WithRoute Route
  { page : Int
  , taskModel : TaskPage.Model
  , loginModel : LoginPage.Model
  }


type Action
    = NoOp
    | TaskPageAction TaskPage.Action
    | LoginPageAction LoginPage.Action
    | RouterAction (TransitRouter.Action Route)
