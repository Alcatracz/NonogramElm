module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, option, p, select, span, table, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Ports exposing (..)
import Random as Random
import Round exposing (round)
import Style exposing (..)
import TestData exposing (..)
import Types exposing (..)


type alias Model =
    { progress : Float
    , mistakes : Int
    , hints : Hints
    , field : Array (Array Cell)
    , solution : Array (Array Bool)
    , state : State
    , rowColSize : Int
    , gameMode : Mode
    , rowSizeInput : Int
    }


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = updateWithStorage
        , view = view
        }


defaultRowColSize : Int
defaultRowColSize =
    5


defaultFrameSize : Float
defaultFrameSize =
    500


init : E.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue decoder flags of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( { progress = 0, mistakes = 0, hints = resetHints defaultRowColSize, field = resetField defaultRowColSize, solution = resetSolution defaultRowColSize, state = Running, rowColSize = defaultRowColSize, gameMode = Classic, rowSizeInput = defaultRowColSize }, generateNewGame defaultRowColSize )


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


hintsFromRow : List Bool -> List Int -> List Int
hintsFromRow list output =
    case list of
        x :: rest ->
            if x then
                case output of
                    y :: reste ->
                        hintsFromRow rest (y + 1 :: reste)

                    [] ->
                        output

            else
                hintsFromRow rest (0 :: output)

        [] ->
            output


generateHintsFromSolution : Array (Array Bool) -> Hints
generateHintsFromSolution solution =
    let
        rows =
            Array.toList <| Array.map (\value -> List.map (\val -> ( val, False )) (List.filter (\val -> val /= 0) (List.reverse (hintsFromRow (Array.toList value) [ 0 ])))) solution

        cols =
            Array.toList <| Array.map (\value -> List.map (\val -> ( val, False )) (List.filter (\val -> val /= 0) (hintsFromRow (Array.toList value) [ 0 ]))) (reverseColRows solution True)
    in
    { rows = rows, cols = cols }


reverseColRows : Array (Array a) -> a -> Array (Array a)
reverseColRows list default =
    let
        another : Int -> Array a -> a
        another index value =
            case Array.get index value of
                Just n ->
                    n

                Nothing ->
                    default

        inner : Int -> Array a -> Array (Array a) -> Array a
        inner index value li =
            case Array.get index value of
                Just _ ->
                    Array.map (\va -> another index va) li

                Nothing ->
                    value
    in
    Array.indexedMap (\index value -> Array.fromList (Array.foldl (::) [] (inner index value list))) list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            case model.gameMode of
                Arcade ->
                    ( { model | progress = 0, mistakes = 0, field = resetField 2, rowColSize = 2 }, generateNewGame 2 )

                _ ->
                    ( { model | progress = 0, mistakes = 0, field = resetField model.rowSizeInput, rowColSize = model.rowSizeInput }, generateNewGame model.rowSizeInput )

        NextLevel ->
            ( { model | progress = 0, field = resetField (model.rowColSize + 1) }, generateNewGame (model.rowColSize + 1) )

        GenerateGame newSolution ->
            let
                newHints =
                    generateHintsFromSolution newSolution
            in
            ( { model | state = Running, hints = newHints, solution = newSolution }, Cmd.none )

        ChangeRowColSize newValue ->
            ( { model | rowSizeInput = newValue }, Cmd.none )

        ChangeGameMode newMode ->
            ( { model | gameMode = newMode }, Cmd.none )

        Solve ->
            ( { model | field = solveMultiSolution model.solution model.field, state = Finished }, Cmd.none )

        Click x y leftClick ->
            let
                correctguess =
                    checkGuess x y model.solution leftClick

                newField =
                    case model.gameMode of
                        MultiSolution ->
                            if leftClick then
                                markAs x y model.field CorrectTrue

                            else
                                markAs x y model.field CorrectFalse

                        _ ->
                            if leftClick == True && correctguess == True then
                                markAs x y model.field CorrectTrue

                            else if leftClick == False && correctguess == True then
                                markAs x y model.field CorrectFalse

                            else if correctguess == False && leftClick == False then
                                markAs x y model.field IncorrectTrue

                            else
                                markAs x y model.field IncorrectFalse

                newHints =
                    updateHintsfromField newField model.hints

                newProgress =
                    (toFloat (sumOfElement CorrectTrue newField + sumOfElement IncorrectTrue newField) / toFloat (sumOfElement True model.solution)) * 100
            in
            case model.gameMode of
                MultiSolution ->
                    ( { model | field = newField, hints = newHints }, Cmd.none )

                _ ->
                    if newProgress == 100 then
                        case model.gameMode of
                            Arcade ->
                                ( { model | progress = 0, rowColSize = model.rowColSize + 1, field = resetField (model.rowColSize + 1) }, generateNewGame (model.rowColSize + 1) )

                            _ ->
                                ( { model | field = newField, state = Finished, progress = newProgress }, Cmd.none )

                    else if correctguess then
                        ( { model | progress = newProgress, field = newField, hints = newHints }, Cmd.none )

                    else
                        ( { model | mistakes = model.mistakes + 1, hints = newHints, field = newField, progress = newProgress }, Cmd.none )


updateHintsfromField : Array (Array Cell) -> Hints -> Hints
updateHintsfromField field hints =
    let
        inner : Int -> List ( Int, Bool ) -> Array (Array Cell) -> List ( Int, Bool )
        inner index li fi =
            case Array.get index fi of
                Just n ->
                    let
                        wat =
                            another (Array.toList n) []
                    in
                    if List.length wat == Array.length n then
                        List.map (\( v, _ ) -> ( v, True )) li

                    else
                        li

                Nothing ->
                    li

        another : List Cell -> List Int -> List Int
        another row list =
            case row of
                x :: rest ->
                    if x == Blank then
                        list

                    else if x == CorrectTrue || x == CorrectFalse then
                        another rest (1 :: list)

                    else
                        another rest (0 :: list)

                [] ->
                    list
    in
    { rows = List.indexedMap (\ind rowHints -> inner ind rowHints field) hints.rows
    , cols = List.indexedMap (\ind rowHints -> inner ind rowHints (reverseColRows field Blank)) hints.cols
    }


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel), cmds ]
    )


solveMultiSolution : Array (Array Bool) -> Array (Array Cell) -> Array (Array Cell)
solveMultiSolution solution field =
    let
        another : Int -> Int -> Cell -> Cell
        another rI cI val =
            case Array.get rI solution of
                Just n ->
                    case Array.get cI n of
                        Just m ->
                            if m && val == CorrectFalse then
                                IncorrectTrue

                            else if m == False && val == CorrectTrue then
                                IncorrectFalse

                            else if val == Blank && m == True then
                                IncorrectTrue

                            else
                                val

                        Nothing ->
                            val

                Nothing ->
                    val

        inner : Array Cell -> Int -> Array Cell
        inner li rowIndex =
            Array.indexedMap (\colIndex value -> another rowIndex colIndex value) li
    in
    Array.indexedMap (\index row -> inner row index) field


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


checkGuess : Int -> Int -> Array (Array Bool) -> Bool -> Bool
checkGuess x y solution markedAs =
    case Array.get x solution of
        Just n ->
            case Array.get y n of
                Just m ->
                    m == markedAs

                Nothing ->
                    False

        Nothing ->
            False


colHintsToView : Hints -> Float -> Html Msg
colHintsToView { cols } size =
    let
        long =
            size * toFloat (ceiling (toFloat (List.length cols) / 5))
    in
    tr (trStyle False) (td (hintTdStyle "column" ( long, long )) [] :: List.map (\list -> Html.td (hintTdStyle "column" ( long, size )) (hintsToView list)) cols)


hintsToView : List ( Int, Bool ) -> List (Html Msg)
hintsToView list =
    List.map (\( value, done ) -> div (hintStyle done) [ text (String.fromInt value) ]) list


cellsToView : Int -> Array Cell -> State -> Float -> Mode -> List (Html Msg)
cellsToView rowIndex list state size mode =
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
                    if state == Finished || value /= Blank && mode /= MultiSolution then
                        cellTdStyle value size (modBy 5 (y + 1) == 0) state

                    else
                        Mouse.onContextMenu (\_ -> Click rowIndex y False) :: Mouse.onClick (\_ -> Click rowIndex y True) :: cellTdStyle value size (modBy 5 (y + 1) == 0) state
            in
            td atribu [ text texto ]
    in
    Array.toList (Array.indexedMap (\index value -> attribu index value) list)


rowToView : State -> Int -> List ( Int, Bool ) -> Array Cell -> Float -> Mode -> Html Msg
rowToView state rowIndex hints cells size mode =
    let
        long =
            size * toFloat (ceiling (toFloat (Array.length cells) / 5))
    in
    Html.tr (trStyle (modBy 5 (rowIndex + 1) == 0)) (td (hintTdStyle "row" ( long, size )) (hintsToView hints) :: cellsToView rowIndex cells state size mode)


fieldToView : Int -> List (Html Msg) -> DoubleList ( Int, Bool ) -> Array (Array Cell) -> State -> Float -> Mode -> List (Html Msg)
fieldToView index list hints field state size mode =
    case hints of
        x :: rest ->
            case Array.get 0 field of
                Just y ->
                    rowToView state index x y size mode :: fieldToView (index + 1) list rest (Array.slice 1 (Array.length field) field) state size mode

                Nothing ->
                    list

        [] ->
            list


gameToView : Model -> List (Html Msg)
gameToView model =
    let
        tdSize =
            defaultFrameSize / toFloat (model.rowColSize + 1)
    in
    colHintsToView model.hints tdSize :: fieldToView 0 [] model.hints.rows model.field model.state tdSize model.gameMode


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

                    "multiSolution" ->
                        D.succeed MultiSolution

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
        [ Html.div sideContainerStyle
            [ h1 [] [ text "Nonogramm" ]
            , h3 [] [ text "by Jan-Ole Claußen" ]
            , p [] [ text "Rules: Left click reveals a field, Right click marks it as empty." ]
            , h3 [] [ text "Spiel" ]
            , case model.gameMode of
                MultiSolution ->
                    button [ onClick Solve ] [ text "Solve" ]

                _ ->
                    p []
                        [ span [] [ text "Progress: " ]
                        , span [] [ text (round 1 model.progress) ]
                        ]
            , p []
                [ span [] [ text "Mistakes: " ]
                , span (mistakesStyle model.mistakes) [ text (String.fromInt model.mistakes) ]
                ]
            , select [ on "change" (D.map ChangeGameMode targetValueModeDecoder) ]
                [ option [ value "classic", selected (model.gameMode == Classic) ] [ text "Classic" ]
                , option [ value "arcade", selected (model.gameMode == Arcade) ] [ text "Arcade" ]
                , option [ value "multiSolution", selected (model.gameMode == MultiSolution) ] [ text "Multiple Solutions" ]
                ]
            , case model.gameMode of
                Arcade ->
                    div [] []

                _ ->
                    select [ on "change" (D.map ChangeRowColSize targetValueSizeDecoder) ]
                        [ option [ value "5x5", selected (model.rowColSize == 5) ] [ text "5x5" ]
                        , option [ value "10x10", selected (model.rowColSize == 10) ] [ text "10x10" ]
                        , option [ value "15x15", selected (model.rowColSize == 15) ] [ text "15x15" ]
                        , option [ value "20x20", selected (model.rowColSize == 20) ] [ text "20x20" ]
                        ]
            , button [ onClick NewGame ] [ text "New Game" ]
            ]
        , div contentContainerStyle
            [ table (tableStyle model.state)
                (gameToView model)
            ]
        ]


encode : Model -> E.Value
encode model =
    E.object
        [ ( "progress", E.float model.progress )
        , ( "mistakes", E.int model.mistakes )
        , ( "rowColsize", E.int model.rowColSize )
        , ( "rowSizeInput", E.int model.rowSizeInput )
        , ( "state", stateEncoder model.state )
        , ( "gameMode", gameModeEncoder model.gameMode )
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
        (D.field "rows" (D.list (D.list tupleDecoder)))
        (D.field "cols" (D.list (D.list tupleDecoder)))


tupleDecoder : D.Decoder ( Int, Bool )
tupleDecoder =
    D.map2 Tuple.pair
        (D.field "hint" D.int)
        (D.field "vis" D.bool)


hintsEncoder : Hints -> E.Value
hintsEncoder hints =
    E.object
        [ ( "rows", E.list (E.list tupleEncoder) hints.rows )
        , ( "cols", E.list (E.list tupleEncoder) hints.cols )
        ]


tupleEncoder : ( Int, Bool ) -> E.Value
tupleEncoder ( hint, vis ) =
    E.object [ ( "hint", E.int hint ), ( "vis", E.bool vis ) ]
