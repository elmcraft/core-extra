module TripleTests exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe)
import Triple.Extra


suite : Test
suite =
    describe "Triple.Extra"
        [ Test.fuzz (Fuzz.triple Fuzz.int Fuzz.int Fuzz.int) "sortWith behaves like list sort" <|
            \triple ->
                triple
                    |> Triple.Extra.toList
                    |> List.sort
                    |> Expect.equal (Triple.Extra.sortWith compare triple |> Triple.Extra.toList)
        ]
