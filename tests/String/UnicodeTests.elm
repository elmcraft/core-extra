module String.UnicodeTests exposing (unicodeTests)

import Char
import Expect
import Fuzz exposing (Fuzzer)
import String
import String.Extra exposing (..)
import Test exposing (Test, describe, fuzz, test)


bmpCodePointFuzzer : Fuzzer Int
bmpCodePointFuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.intRange 0 0xD7FF )
        , ( 1, Fuzz.intRange 0xE000 0xFFFF )
        ]


unicodeStringFuzzer : Fuzzer String
unicodeStringFuzzer =
    let
        singletonFuzzer =
            bmpCodePointFuzzer
                |> Fuzz.map (\codePoint -> [ Char.fromCode codePoint ])

        leadingSurrogateFuzzer =
            Fuzz.intRange 0xD800 0xDBFF

        trailingSurrogateFuzzer =
            Fuzz.intRange 0xDC00 0xDFFF

        surrogatePairFuzzer =
            Fuzz.map2
                (\leading trailing -> [ Char.fromCode leading, Char.fromCode trailing ])
                leadingSurrogateFuzzer
                trailingSurrogateFuzzer

        sublistFuzzer =
            Fuzz.frequency
                [ ( 1, singletonFuzzer )
                , ( 1, surrogatePairFuzzer )
                ]
    in
    Fuzz.list sublistFuzzer
        |> Fuzz.map List.concat
        |> Fuzz.map String.fromList


codePointFuzzer : Fuzzer Int
codePointFuzzer =
    let
        astralCodePointFuzzer =
            Fuzz.intRange 0x00010000 0x0010FFFF
    in
    Fuzz.frequency
        [ ( 1, bmpCodePointFuzzer )
        , ( 1, astralCodePointFuzzer )
        ]


expectedStringLength : List Int -> Int
expectedStringLength codePoints =
    let
        numCodeUnits codePoint =
            if codePoint <= 0xFFFF then
                1

            else
                2
    in
    codePoints |> List.map numCodeUnits |> List.sum


hardCodedTestCases : List ( String, List Int )
hardCodedTestCases =
    [ ( "", [] )
    , ( "©§π", [ 169, 167, 960 ] )
    , ( "💩!", [ 128169, 33 ] )
    , ( "abc", [ 97, 98, 99 ] )
    ]


unicodeTests : Test
unicodeTests =
    describe "unicode"
        [ fuzz unicodeStringFuzzer "fromCodePoints is inverse of toCodePoints" <|
            \string ->
                fromCodePoints (toCodePoints string)
                    |> Expect.equal string
        , fuzz (Fuzz.list codePointFuzzer) "toCodePoints is inverse of fromCodePoints" <|
            \codePoints ->
                toCodePoints (fromCodePoints codePoints)
                    |> Expect.equal codePoints
        , fuzz (Fuzz.list codePointFuzzer) "string length is greater than or equal to number of code points" <|
            \codePoints ->
                String.length (fromCodePoints codePoints)
                    |> Expect.atLeast (List.length codePoints)
        , fuzz unicodeStringFuzzer "number of code points is less than or equal to string length" <|
            \string ->
                List.length (toCodePoints string)
                    |> Expect.atMost (String.length string)
        , fuzz (Fuzz.list codePointFuzzer) "encoded string length is as expected" <|
            \codePoints ->
                String.length (fromCodePoints codePoints)
                    |> Expect.equal (expectedStringLength codePoints)
        , describe "toCodePoints works as expected on hard-coded test cases"
            (hardCodedTestCases
                |> List.indexedMap
                    (\index ( string, codePoints ) ->
                        test ("toCodePoints works properly - test case " ++ Debug.toString index)
                            (\() -> toCodePoints string |> Expect.equal codePoints)
                    )
            )
        , describe "fromCodePoints works as expected on hard-coded test cases"
            (hardCodedTestCases
                |> List.indexedMap
                    (\index ( string, codePoints ) ->
                        test ("fromCodePoints works properly - test case " ++ Debug.toString index)
                            (\() -> fromCodePoints codePoints |> Expect.equal string)
                    )
            )
        ]
