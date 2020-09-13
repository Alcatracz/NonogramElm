port module Ports exposing (..)

import Html.Attributes exposing (..)
import Json.Encode as E
import Types exposing (..)


port setStorage : E.Value -> Cmd msg
