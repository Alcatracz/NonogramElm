port module Ports exposing (..)

import Json.Decode as D
import Json.Encode as E
import Types exposing (..)
import Html.Attributes exposing (..)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)

port setStorage : E.Value -> Cmd msg

