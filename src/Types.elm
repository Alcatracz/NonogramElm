module Types exposing (..)


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
