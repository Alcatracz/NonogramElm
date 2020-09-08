module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, input, option, p, select, span, table, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Random as Random
import Round exposing (round)
import Style exposing (..)
import TestData exposing (..)
import Types exposing (..)


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


defaultRowColSize : Int
defaultRowColSize =
    5


type alias Model =
    { progress : Float
    , mistakes : Int
    , hints : Hints
    , field : Array (Array Cell)
    , solution : Array (Array Bool)
    , state : State
    , rowColSize : Int
    , gameMode : Mode
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { progress = 0, mistakes = 0, hints = resetHints defaultRowColSize, field = resetField defaultRowColSize, solution = resetSolution defaultRowColSize, state = Running, rowColSize = defaultRowColSize, gameMode = Classic }, generateNewGame defaultRowColSize )


resetField : Int -> Array (Array Cell)
resetField n =
    Array.repeat n (Array.repeat n Blank)


resetHints : Int -> Hints
resetHints n =
    { rows = List.repeat n []
    , cols = List.repeat n []
    }


randBool : Random.Generator Bool
randBool =
    Random.map (\x -> x == 1) (Random.int 0 1)


resetSolution : Int -> Array (Array Bool)
resetSolution n =
    Array.repeat n (Array.repeat n False)


solutionGenerator : Int -> Random.Generator (Array (Array Bool))
solutionGenerator n =
    Random.map (\val -> chunk val n) (Random.list (n * n) randBool)


chunk : List a -> Int -> Array (Array a)
chunk list n =
    let
        inner : List a -> Array (Array a) -> Array (Array a)
        inner li wa =
            case li of
                [] ->
                    wa

                _ ->
                    inner (List.drop n li) (Array.push (Array.fromList (List.take n li)) wa)
    in
    if List.length list <= n then
        Array.fromList [ Array.fromList list ]

    else
        inner list (Array.fromList [])


generateNewGame : Int -> Cmd Msg
generateNewGame n =
    Random.generate GenerateGame (solutionGenerator n)


rowWut : List Bool -> List Int -> List Int
rowWut list wa =
    case list of
        x :: rest ->
            if x == True then
                case wa of
                    y :: reste ->
                        rowWut rest (y + 1 :: reste)

                    [] ->
                        wa

            else
                rowWut rest (0 :: wa)

        [] ->
            wa


generateHintsFromSolution : Array (Array Bool) -> Hints
generateHintsFromSolution solution =
    let
        rows =
            Array.toList <| Array.map (\value -> List.filter (\val -> val /= 0) (List.reverse (rowWut (Array.toList value) [ 0 ]))) solution

        cols =
            Array.toList <| Array.map (\value -> List.filter (\val -> val /= 0) (rowWut (Array.toList value) [ 0 ])) (reverseColRows solution)
    in
    { rows = rows, cols = cols }


reverseColRows : Array (Array Bool) -> Array (Array Bool)
reverseColRows list =
    let
        another : Int -> Array Bool -> Bool
        another index value =
            case Array.get index value of
                Just n ->
                    n

                Nothing ->
                    True

        inner : Int -> Array Bool -> Array (Array Bool) -> Array Bool
        inner index value li =
            case Array.get index value of
                Just _ ->
                    Array.map (\va -> another index va) li

                Nothing ->
                    value
    in
    Array.indexedMap (\index value -> Array.fromList (Array.foldl (::) [] (inner index value list))) list


type Msg
    = NewGame
    | Click Int Int Bool
    | GenerateGame (Array (Array Bool))
    | ChangeRowColSize Int
    | ChangeGameMode Mode
    | NextLevel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            case model.gameMode of
                Arcade ->
                    ( { model | progress = 0, mistakes = 0, field = resetField 2, rowColSize = 2 }, generateNewGame 2 )

                _ ->
                    ( { model | progress = 0, mistakes = 0, field = resetField model.rowColSize }, generateNewGame model.rowColSize )

        NextLevel ->
            ( { model | progress = 0, field = resetField (model.rowColSize + 1) }, generateNewGame (model.rowColSize + 1) )

        GenerateGame newSolution ->
            let
                newHints =
                    generateHintsFromSolution newSolution
            in
            ( { model | state = Running, hints = newHints, solution = newSolution }, Cmd.none )

        ChangeRowColSize newValue ->
            ( { model | rowColSize = newValue }, Cmd.none )

        ChangeGameMode newMode ->
            ( { model | gameMode = newMode }, Cmd.none )

        Click x y bo ->
            if checkDat x y model.solution bo then
                let
                    newField =
                        if bo == True then
                            markAs x y model.field CorrectTrue

                        else
                            markAs x y model.field CorrectFalse

                    newProgress =
                        (toFloat (sumOfElement CorrectTrue newField + sumOfElement IncorrectTrue newField) / toFloat (sumOfElement True model.solution)) * 100
                in
                if newProgress == 100 then
                    case model.gameMode of
                        Arcade ->
                            ( { model | progress = 0, rowColSize = model.rowColSize + 1, field = resetField (model.rowColSize + 1) }, generateNewGame (model.rowColSize + 1) )

                        _ ->
                            ( { model | field = newField, state = Done, progress = newProgress }, Cmd.none )

                else
                    ( { model | progress = newProgress, field = newField }, Cmd.none )

            else
                let
                    newField =
                        if bo == True then
                            markAs x y model.field IncorrectFalse

                        else
                            markAs x y model.field IncorrectTrue

                    newProgress =
                        (toFloat (sumOfElement CorrectTrue newField + sumOfElement IncorrectTrue newField) / toFloat (sumOfElement True model.solution)) * 100
                in
                if newProgress == 100 then
                    case model.gameMode of
                        Arcade ->
                            ( { model | mistakes = model.mistakes + 1, progress = 0, field = resetField (model.rowColSize + 1), rowColSize = model.rowColSize + 1 }, generateNewGame (model.rowColSize + 1) )

                        _ ->
                            ( { model | mistakes = model.mistakes + 1, field = newField, state = Done, progress = newProgress }, Cmd.none )

                else
                    ( { model | mistakes = model.mistakes + 1, field = newField, progress = newProgress }, Cmd.none )


sumOfElement : a -> Array (Array a) -> Int
sumOfElement ele array =
    List.length (List.filter (\value -> value == ele) (flatten array))


flatten : Array (Array a) -> List a
flatten array =
    List.concat <| (Array.map Array.toList array |> Array.toList)


markAs : Int -> Int -> Array (Array Cell) -> Cell -> Array (Array Cell)
markAs x y field value =
    case Array.get x field of
        Just n ->
            Array.set x (Array.set y value n) field

        Nothing ->
            field


checkDat : Int -> Int -> Array (Array Bool) -> Bool -> Bool
checkDat x y solution markedAs =
    case Array.get x solution of
        Just n ->
            case Array.get y n of
                Just m ->
                    m == markedAs

                Nothing ->
                    False

        Nothing ->
            False


colHintsToView : Hints -> Html Msg
colHintsToView { cols } =
    tr trStyle (List.map (\list -> Html.td (hintTdStyle "column") (hintsToView list)) ([] :: cols))


hintsToView : List Int -> List (Html Msg)
hintsToView list =
    List.map (\value -> div [] [ text (String.fromInt value) ]) list


rowToView : Int -> Array Cell -> State -> List (Html Msg)
rowToView rowIndex list state =
    let
        attribu : Int -> Cell -> Html Msg
        attribu y value =
            let
                texto =
                    if value == IncorrectTrue || value == IncorrectFalse then
                        "X"

                    else
                        ""

                atribu =
                    if state == Done || value /= Blank then
                        cellTdStyle value

                    else
                        Mouse.onContextMenu (\_ -> Click rowIndex y False) :: Mouse.onClick (\_ -> Click rowIndex y True) :: cellTdStyle value
            in
            td atribu [ text texto ]
    in
    Array.toList (Array.indexedMap (\index value -> attribu index value) list)


rowtt : State -> Int -> List Int -> Array Cell -> Html Msg
rowtt state rowIndex hints cells =
    Html.tr trStyle (td (hintTdStyle "row") (hintsToView hints) :: rowToView rowIndex cells state)


fieldToView : Int -> List (Html Msg) -> DoubleList Int -> Array (Array Cell) -> State -> List (Html Msg)
fieldToView index li hints field state =
    case hints of
        x :: rest ->
            case Array.get 0 field of
                Just y ->
                    rowtt state index x y :: fieldToView (index + 1) li rest (Array.slice 1 (Array.length field) field) state

                Nothing ->
                    li

        [] ->
            li


gameToView : Model -> List (Html Msg)
gameToView model =
    let
        wat =
            colHintsToView model.hints

        table =
            fieldToView 0 [] model.hints.rows model.field model.state
    in
    wat :: table


targetValueModeDecoder : D.Decoder Mode
targetValueModeDecoder =
    targetValue
        |> D.andThen
            (\val ->
                case val of
                    "classic" ->
                        D.succeed Classic

                    "arcade" ->
                        D.succeed Arcade

                    _ ->
                        D.succeed Classic
            )


targetValueSizeDecoder : D.Decoder Int
targetValueSizeDecoder =
    targetValue
        |> D.andThen
            (\val ->
                case val of
                    "5x5" ->
                        D.succeed 5

                    "10x10" ->
                        D.succeed 10

                    "15x15" ->
                        D.succeed 15

                    "20x20" ->
                        D.succeed 20

                    _ ->
                        D.succeed 5
            )


view : Model -> Html Msg
view model =
    Html.div containerStyle
        [ Html.div headerContainerStyle
            [ h1 [] [ text "Nonogramm" ]
            , h3 [] [ text "by Jan-Ole Claußen" ]
            , p [] [ text "Rules: Left click reveals a field, Right click marks it as empty." ]
            ]
        , Html.div contentContainerStyle
            [ Html.div sideContainerStyle
                [ h3 [] [ text "Spiel" ]
                , p []
                    [ span [] [ text "Progress: " ]
                    , span [] [ text (round 1 model.progress) ]
                    ]
                , p []
                    [ span [] [ text "Mistakes: " ]
                    , span [] [ text (String.fromInt model.mistakes) ]
                    ]
                , select [ on "change" (D.map ChangeGameMode targetValueModeDecoder) ]
                    [ option [ value "classic" ] [ text "Classic" ]
                    , option [ value "arcade" ] [ text "Arcade" ]
                    ]
                , case model.gameMode of
                    Arcade ->
                        div [] []

                    _ ->
                        select [ on "change" (D.map ChangeRowColSize targetValueSizeDecoder) ]
                            [ option [ value "5x5" ] [ text "5x5" ]
                            , option [ value "10x10" ] [ text "10x10" ]
                            , option [ value "15x15" ] [ text "15x15" ]
                            , option [ value "20x20" ] [ text "20x20" ]
                            ]
                , button [ onClick NewGame ] [ text "New Game" ]
                ]
            , div []
                [ case model.state of
                    Done ->
                        div [] [ text "Fertig" ]

                    _ ->
                        div [] []
                , table (tableStyle model.state)
                    (gameToView model)
                ]
            ]
        ]
