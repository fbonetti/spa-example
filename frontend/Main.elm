module Main where

import Effects exposing (Never)
import Task
import Signal
import StartApp
import Html exposing (Html)
import Date exposing (Date)

import Update exposing (init, update, actions)
import View exposing (view)


port initialPath : String
port currentTime : Float

app =
  StartApp.start
    { init = init initialPath (Date.fromTime currentTime)
    , update = update
    , view = view
    , inputs = [ actions ]
    }

main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
