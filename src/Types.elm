module Types exposing (..)

import Array exposing (Array)


type Cell
    = Blank
    | CorrectTrue
    | CorrectFalse
    | IncorrectTrue
    | IncorrectFalse


type State
    = Finished
    | Running


type alias DoubleList a =
    List (List a)


type alias Hints =
    { rows : DoubleList Int
    , cols : DoubleList Int
    }


type Mode
    = Classic
    | Arcade
    | MultiSolution





type Msg
    = NewGame
    | Click Int Int Bool
    | GenerateGame (Array (Array Bool))
    | ChangeRowColSize Int
    | ChangeGameMode Mode
    | NextLevel
    | Solve
