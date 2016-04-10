module Pages.Logout where

import Http.Extra exposing (..)
import Task
import Effects exposing (Effects)
import Json.Decode exposing ((:=))

postLogout : Effects (Result (Error String) (Response String))
postLogout =
  let
    successDecoder = ("message" := Json.Decode.string)
    errorDecoder = ("error" := Json.Decode.string)
  in
    post "/api/v1/logout"
      |> withHeader "Content-Type" "application/json"
      |> withCredentials
      |> send (jsonReader successDecoder) (jsonReader errorDecoder)
      |> Task.toResult
      |> Effects.task