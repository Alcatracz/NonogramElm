module Style exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)


containerStyle : List (Attribute msg)
containerStyle =
    [ style "display" "flex"
    , style "flex" "1"
    , style "flex-direction" "column"
    , style "font-family" "Helvetica, sans-serif"
    , style "background-color" "#eeeeee"
    ]


headerContainerStyle : List (Attribute msg)
headerContainerStyle =
    [ style "display" "flex"
    , style "flex" "1"
    , style "height" "400px"
    , style "padding" "1em"
    , style "flex-direction" "column"
    ]


contentContainerStyle : List (Attribute msg)
contentContainerStyle =
    [ style "display" "flex"
    , style "flex" "3"
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
    ]


tableStyle : State -> List (Attribute msg)
tableStyle state =
    if state == Done then
        [ style "pointer-events" "none" ]

    else
        []


hintTdStyle : String -> List (Attribute msg)
hintTdStyle direction =
    let
        custom : List (Attribute msg)
        custom =
            if direction == "row" then
                [ style "flex-direction" "row"
                , style "border-left" "none"
                ]

            else
                [ style "flex-direction" "column"
                , style "border-top" "none"
                ]
    in
    [ style "width" "50px"
    , style "height" "50px"
    , style "border" "1px solid black"
    , style "display" "flex"
    , style "justify-content" "space-evenly"
    , style "align-items" "center"
    ]
        ++ custom


cellTdStyle : Cell -> List (Attribute msg)
cellTdStyle cell =
    let
        custom : List (Attribute msg)
        custom =
            case cell of
                Blank ->
                    [ style "cursor" "pointer"
                    ]

                CorrectTrue ->
                    [ style "background-color" "green" ]

                CorrectFalse ->
                    [ style "background-color" "grey" ]

                IncorrectTrue ->
                    [ style "background-color" "green" ]

                IncorrectFalse ->
                    [ style "background-color" "grey" ]
    in
    [ style "width" "50px"
    , style "height" "50px"
    , style "border" "1px solid lightgrey"
    ]
        ++ custom
