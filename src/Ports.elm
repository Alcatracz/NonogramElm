port module Ports exposing (..)

import Json.Decode as D
import Json.Encode as E
import Types exposing (..)
import Html.Attributes exposing (..)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)

port setStorage : E.Value -> Cmd msg

encode : Model -> E.Value
encode model =
    E.object
        [ ( "progress", E.float model.progress )
        , ( "mistakes", E.int model.mistakes )
        , ( "rowColsize", E.int model.rowColSize )
        , ( "rowSizeInput", E.int model.rowSizeInput )
        , ( "state", stateEncoder model.state )
        , ( "gameMode"
          , gameModeEncoder model.gameMode
          )
        , ( "hints", hintsEncoder model.hints )
        , ( "field", E.array (E.array cellEncoder) model.field )
        , ( "solution", E.array (E.array E.bool) model.solution )
        ]


decoder : D.Decoder Model
decoder =
    D.succeed Model
        |> required "progress" D.float
        |> required "mistakes" D.int
        |> required "hints" hintsDecoder
        |> required "field" (D.array (D.array cellDecoder))
        |> required "solution" (D.array (D.array D.bool))
        |> required "state" stateDecoder
        |> required "rowColsize" D.int
        |> required "gameMode" gameModeDecoder
        |> required "rowSizeInput" D.int


cellDecoder : D.Decoder Cell
cellDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "blank" ->
                        D.succeed Blank

                    "CorrectTrue" ->
                        D.succeed CorrectTrue

                    "CorrectFalse" ->
                        D.succeed CorrectFalse

                    "IncorrectTrue" ->
                        D.succeed IncorrectTrue

                    "IncorrectFalse" ->
                        D.succeed IncorrectFalse

                    _ ->
                        D.succeed Blank
            )


cellEncoder : Cell -> E.Value
cellEncoder cell =
    E.string <|
        case cell of
            Blank ->
                "blank"

            CorrectTrue ->
                "CorrectTrue"

            CorrectFalse ->
                "CorrectFalse"

            IncorrectTrue ->
                "IncorrectTrue"

            IncorrectFalse ->
                "IncorrectFalse"


gameModeDecoder : D.Decoder Mode
gameModeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "classic" ->
                        D.succeed Classic

                    "arcade" ->
                        D.succeed Arcade

                    "multi" ->
                        D.succeed MultiSolution

                    _ ->
                        D.succeed Classic
            )


gameModeEncoder : Mode -> E.Value
gameModeEncoder mode =
    E.string <|
        case mode of
            Classic ->
                "classic"

            Arcade ->
                "arcade"

            MultiSolution ->
                "multi"


stateDecoder : D.Decoder State
stateDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "running" ->
                        D.succeed Running

                    "finished" ->
                        D.succeed Finished

                    _ ->
                        D.succeed Running
            )


stateEncoder : State -> E.Value
stateEncoder state =
    E.string <|
        case state of
            Running ->
                "running"

            Finished ->
                "finished"


hintsDecoder : D.Decoder Hints
hintsDecoder =
    D.map2 Hints
        (D.field "rows" (D.list (D.list D.int)))
        (D.field "cols" (D.list (D.list D.int)))


hintsEncoder : Hints -> E.Value
hintsEncoder hints =
    E.object
        [ ( "rows", E.list (E.list E.int) hints.rows )
        , ( "cols", E.list (E.list E.int) hints.cols )
        ]
