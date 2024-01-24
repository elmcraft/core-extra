module String.RemoveDiacriticsTests exposing (removeDiacriticsTests)

import Dict
import Expect exposing (equal)
import Fuzz
import String.Diacritics exposing (lookupTable)
import String.Extra exposing (removeDiacritics)
import Test exposing (Test, describe, fuzz, test)


removeDiacriticsTests : Test
removeDiacriticsTests =
    describe "String.Normalize.removeDiacritics"
        [ test "removes lowercase accents" <|
            \_ ->
                removeDiacritics "éeaèüàäö"
                    |> equal "eeaeuaao"
        , test "removes uppercase accents" <|
            \_ ->
                removeDiacritics "ÉEAÈÜÀÄÖ"
                    |> equal "EEAEUAAO"
        , test "removes ligatures" <|
            \_ ->
                removeDiacritics "Æƕ"
                    |> equal "AEhv"
        , test "normalizes a sentence" <|
            \_ ->
                removeDiacritics "La liberté commence où l'ignorance finit."
                    |> equal "La liberte commence ou l'ignorance finit."
        , test "don't touch punctuation" <|
            \_ ->
                removeDiacritics "é()/& abc"
                    |> equal "e()/& abc"
        , test "don't touch non latin characters" <|
            \_ ->
                removeDiacritics "こんにちは"
                    |> equal "こんにちは"
        , fuzz Fuzz.asciiString "don't touch ASCII" <|
            \randomAscii ->
                removeDiacritics randomAscii
                    |> equal randomAscii
        , fuzz onlyDiacritics "always change diacritics" <|
            \randomDiacritics ->
                removeDiacritics randomDiacritics
                    |> (if randomDiacritics == "" then
                            Expect.equal ""

                        else
                            Expect.notEqual randomDiacritics
                       )
        , fuzz
            withDiacritics
            "second pass does nothing"
          <|
            \randomString ->
                let
                    firstPass =
                        removeDiacritics randomString

                    secondPass =
                        removeDiacritics firstPass
                in
                Expect.equal firstPass secondPass
        , fuzz
            withDiacritics
            "no regression with optimized version"
          <|
            \randomString ->
                let
                    old =
                        oldRemoveDiacritics randomString

                    new =
                        removeDiacritics randomString
                in
                Expect.equal old new
        ]


oldRemoveDiacritics : String -> String
oldRemoveDiacritics str =
    let
        replace c result =
            case Dict.get c lookupTable of
                Just candidate ->
                    result ++ candidate

                Nothing ->
                    result ++ String.fromChar c
    in
    String.foldl replace "" str


withDiacritics : Fuzz.Fuzzer String
withDiacritics =
    Fuzz.oneOf [ diacritic, Fuzz.char ]
        |> Fuzz.list
        |> Fuzz.map String.fromList


onlyDiacritics : Fuzz.Fuzzer String
onlyDiacritics =
    Fuzz.map String.fromList (Fuzz.list diacritic)


diacritic : Fuzz.Fuzzer Char
diacritic =
    lookupTable
        |> Dict.keys
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
