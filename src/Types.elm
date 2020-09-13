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
    { rows : DoubleList ( Int, Bool )
    , cols : DoubleList ( Int, Bool )
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
    | Hover Int Int Bool
