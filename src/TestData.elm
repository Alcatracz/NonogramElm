module TestData exposing (..)

import Array exposing (..)
import Types exposing (..)


solutionTestData : Array (Array Bool)
solutionTestData =
    fromList [ fromList [ True, True, True, True, True ], fromList [ False, False, False, False, False ], fromList [ True, True, False, True, True ], fromList [ True, True, False, False, False ], fromList [ True, False, True, False, True ] ]


hintsTestData : Hints
hintsTestData =
    { rows = [ [ 5 ], [ 0 ], [ 2, 2 ], [ 2 ], [ 1, 1, 1 ] ]
    , cols = [ [ 1, 3 ], [ 1, 2 ], [ 1, 1 ], [ 1, 1 ], [ 1, 1, 1 ] ]
    }
