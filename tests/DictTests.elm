module DictTests exposing (suite)

import Dict
import Dict.Extra
import Expect
import Fuzz exposing (Fuzzer)
import Set
import Set.Extra
import Test exposing (Test, describe)


dictFuzzer : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict.Dict comparable v)
dictFuzzer keyFuzzer valueFuzzer =
    Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair keyFuzzer valueFuzzer))


suite : Test
suite =
    describe "Dict.Extra"
        [ describe "invertAll"
            [ Test.fuzz (dictFuzzer Fuzz.string Fuzz.int) "does not loose information" <|
                \dict ->
                    dict
                        |> Dict.Extra.invertAll
                        |> Dict.map (always Set.size)
                        |> Dict.values
                        |> List.sum
                        |> Expect.equal (Dict.size dict)
            , Test.fuzz (dictFuzzer Fuzz.string Fuzz.int) "the values are keys into the original dictionary" <|
                \dict ->
                    dict
                        |> Dict.Extra.invertAll
                        |> Dict.Extra.all
                            (\key vals ->
                                Set.Extra.all (\val -> Dict.get val dict == Just key) vals
                            )
                        |> Expect.equal True
            , Test.fuzz (Fuzz.map Dict.Extra.invert (dictFuzzer Fuzz.string Fuzz.int)) "behaves the same as invert on an already inverted dictionary" <|
                \dict ->
                    dict
                        |> Dict.Extra.invertAll
                        |> Expect.equal (dict |> Dict.Extra.invert |> Dict.map (always Set.singleton))
            ]
        ]
