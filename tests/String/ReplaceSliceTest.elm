module String.ReplaceSliceTest exposing (replaceSliceTest)

import Expect
import Fuzz exposing (Fuzzer)
import String
import String.Extra exposing (..)
import Test exposing (Test, describe, fuzz)


replaceSliceTest : Test
replaceSliceTest =
    describe "replaceSlice"
        [ fuzz replaceSliceProducer "Result contains the substitution string" <|
            \{ string, start, end, sub } ->
                case string of
                    "" ->
                        replaceSlice sub start end string
                            |> Expect.equal sub

                    _ ->
                        replaceSlice sub start end string
                            |> String.contains sub
                            |> Expect.equal True
                            |> Expect.onFail "The slice was not subtituted"
        , fuzz replaceSliceProducer "Result string has the length of the substitution + string after removing the slice" <|
            \{ string, start, end, sub } ->
                case string of
                    "" ->
                        replaceSlice sub start end string
                            |> String.length
                            |> Expect.equal (String.length sub)

                    _ ->
                        replaceSlice sub start end string
                            |> String.length
                            |> Expect.equal ((String.length string - (end - start)) + String.length sub)
        , fuzz replaceSliceProducer "Start of the original string remains the same" <|
            \{ string, start, end, sub } ->
                case string of
                    "" ->
                        replaceSlice sub start end string
                            |> Expect.equal sub

                    _ ->
                        replaceSlice sub start end string
                            |> String.slice 0 start
                            |> Expect.equal (String.slice 0 start string)
        , fuzz replaceSliceProducer "End of the original string remains the same" <|
            \{ string, start, end, sub } ->
                let
                    replaced =
                        replaceSlice sub start end string
                in
                case string of
                    "" ->
                        replaced
                            |> Expect.equal sub

                    _ ->
                        replaced
                            |> String.slice (start + String.length sub) (String.length replaced)
                            |> Expect.equal (String.slice end (String.length string) string)
        ]


replaceSliceProducer : Fuzzer { string : String, sub : String, start : Int, end : Int }
replaceSliceProducer =
    Fuzz.map2
        (\( string, start, end ) sub ->
            { string = string
            , sub = sub
            , start = start
            , end = end
            }
        )
        (Fuzz.string
            |> Fuzz.andThen
                (\string ->
                    (Fuzz.intRange 0 <| String.length string)
                        |> Fuzz.andThen
                            (\start ->
                                (Fuzz.intRange start <| String.length string)
                                    |> Fuzz.map
                                        (\end -> ( string, start, end ))
                            )
                )
        )
        Fuzz.string
