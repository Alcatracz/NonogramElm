module Style exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Types exposing (State(..))


containerStyle : List (Attribute msg)
containerStyle =
    [ style "display" "flex"
    , style "flex" "1"
    , style "flex-direction" "row"
    , style "font-family" "Helvetica, sans-serif"
    , style "background-color" "#eeeeee"
    , style "padding" "3em"
    ]


headerContainerStyle : List (Attribute msg)
headerContainerStyle =
    [ style "display" "flex"
    , style "flex" "1"
    , style "padding" "1em"
    , style "flex-direction" "column"
    ]


contentContainerStyle : List (Attribute msg)
contentContainerStyle =
    [ style "display" "flex"
    , style "flex" "1"
    , style "margin-left" "1em"
    ]


sideContainerStyle : List (Attribute msg)
sideContainerStyle =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "padding" "1em"
    , style "width" "200px"
    ]


trStyle : List (Attribute msg)
trStyle =
    [ style "display" "flex"

    --  , style "flex" "1"
    , style "padding" "0"
    , style "margin" "0"
    , style "border" "0"
    , style "box-sizing" "border-box"
    , style "border-bottom" "1px solid grey"
    ]


tableStyle : State -> List (Attribute msg)
tableStyle state =
    if state == Finished then
        [ style "pointer-events" "none"

        --, style "width" "500px"
        --, style "height" "500px"
        ]

    else
        [ --style "width" "700px"
          --, style "height" "700px"
          style "padding" "0"
        , style "margin" "0"
        , style "border" "0"
        , style "border-collapse" "collapse"
        , style "border-spacing" "0"
        , style "box-sizing" "border-box"

        -- , style "display" "flex"
        --  , style "flex-direction" "column"
        ]


hintTdStyle : String -> ( Float, Float ) -> List (Attribute msg)
hintTdStyle direction ( width, height ) =
    let
        custom : List (Attribute msg)
        custom =
            let
                wid =
                    String.fromFloat width ++ "px"

                hei =
                    String.fromFloat height ++ "px"
            in
            if direction == "row" then
                [ style "flex-direction" "row"
                , style "min-width" "25px"
                , style "border-left" "none"
                , style "width" wid
                , style "height" hei
                ]

            else
                [ style "flex-direction" "column"
                , style "border-top" "none"
                , style "width" hei
                , style "height" wid
                ]
    in
    [ --style "border" "1px solid black"
      style "display" "flex"

    -- , style "flex" "1"
    --  , style "width" "5vw"
    --, style "height" "5vw"
    , style "justify-content" "flex-end"
    , style "align-items" "center"
    , style "padding" "0"
    , style "margin" "0"
    , style "border" "0"
    , style "box-sizing" "border-box"
    , style "border-right" "1px solid grey"
    ]
        ++ custom


cellTdStyle : Cell -> Float -> State -> List (Attribute msg)
cellTdStyle cell size state =
    let
        wid =
            String.fromFloat size ++ "px"

        custom : List (Attribute msg)
        custom =
            case cell of
                Blank ->
                    [ style "cursor" "pointer"
                    ]

                CorrectTrue ->
                    if state == Finished then
                        [ style "background-color" "green" ]

                    else
                        [ style "background-color" "blue" ]

                CorrectFalse ->
                    [ style "background-color" "grey" ]

                IncorrectTrue ->
                    if state == Finished then
                        [ style "background-color" "green" ]

                    else
                        [ style "background-color" "blue" ]

                IncorrectFalse ->
                    [ style "background-color" "grey" ]
    in
    [ --style "border" "1px solid lightgrey"
      style "display" "flex"

    -- , style "flex" "1"
    --, style "width" "5vw"
    --, style "height" "5vw"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "padding" "0"
    , style "margin" "0"
    , style "border" "0"
    , style "box-sizing" "border-box"
    , style "width" wid
    , style "height" wid
    , style "border-right" "1px solid grey"
    , style "text-align" "center"
    , style "color" "red"
    ]
        ++ custom


hintStyle : List (Attribute msg)
hintStyle =
    [ style "padding" "0.25em"
    ]


footerContainerStyle : List (Attribute msg)
footerContainerStyle =
    []


mistakesStyle : Int -> List (Attribute msg)
mistakesStyle m =
    if m /= 0 then
        [ style "color" "red"
        ]

    else
        []
