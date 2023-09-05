module String.CamelizeTest exposing (camelizeTest)

import Expect
import Fuzz exposing (Fuzzer)
import Regex
import String exposing (replace)
import String.Extra exposing (camelize)
import String.TestData as TestData
import Test exposing (Test, describe, fuzz)


camelizeTest : Test
camelizeTest =
    describe "camelize"
        [ fuzz Fuzz.string "It does not contain dashes" <|
            \s ->
                camelize s
                    |> String.contains "-"
                    |> Expect.equal False
                    |> Expect.onFail "Camelize should remove dashes"
        , fuzz Fuzz.string "It does not contain underscores" <|
            \s ->
                camelize s
                    |> String.contains "-"
                    |> Expect.equal False
                    |> Expect.onFail "Camelize should remove underscores"
        , fuzz Fuzz.string "It is the same lowercased string after removing the dashes and spaces" <|
            \s ->
                let
                    expected =
                        replace "-" ""
                            >> replace "_" ""
                            >> Regex.replace (Regex.fromString "\\s+" |> Maybe.withDefault Regex.never) (\_ -> "")
                            >> String.toLower
                in
                camelize s
                    |> String.toLower
                    |> Expect.equal (expected s)
        , fuzz (validWords '-') "The first letter after each dash is capitalized" <|
            \s ->
                camelize s
                    |> Expect.equal (runCamelize "-" s)
        , fuzz (validWords ' ') "The first letter after each space is capitalized" <|
            \s ->
                camelize s
                    |> Expect.equal (runCamelize " " s)
        ]


runCamelize : String -> String -> String
runCamelize separator string =
    string
        |> String.trim
        |> replace (separator ++ separator) separator
        |> String.split separator
        |> List.indexedMap capitalizeOdds
        |> String.concat


capitalizeOdds : Int -> String -> String
capitalizeOdds pos str =
    if pos > 0 then
        String.Extra.toSentenceCase str

    else
        str


validWords : Char -> Fuzzer String
validWords ch =
    TestData.randomStringsWithChars [ ch ]
