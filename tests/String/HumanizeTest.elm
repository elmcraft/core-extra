module String.HumanizeTest exposing (humanizeTest)

import Char
import Expect
import Fuzz exposing (Fuzzer)
import Regex
import String
import String.Extra exposing (humanize)
import String.TestData as TestData
import Test exposing (Test, describe, fuzz, test)
import Tuple exposing (first, second)


humanizeTest : Test
humanizeTest =
    describe "humanize"
        [ test "All uppercase" <|
            \_ ->
                humanize "ALL_UPPERCASE IS FINE"
                    |> Expect.equal "All uppercase is fine"
        , test "Some uppercase" <|
            \_ ->
                humanize "I like HTML"
                    |> Expect.equal "I like html"
        , test "Snake case" <|
            \_ ->
                humanize "this_is_great"
                    |> Expect.equal "This is great"
        , test "Capitalized" <|
            \_ ->
                humanize "ThisIsGreat"
                    |> Expect.equal "This is great"
        , test "Kebab case" <|
            \_ ->
                humanize "this-is-great"
                    |> Expect.equal "This is great"
        , test "Id suffix" <|
            \_ ->
                humanize "author_id"
                    |> Expect.equal "Author"
        , fuzz (validWords []) "It starts with an uppercase letter after trimming" <|
            \s ->
                let
                    expected =
                        String.trim
                            >> String.Extra.toSentenceCase
                            >> String.uncons
                            >> Maybe.map (first >> String.fromChar)
                            >> Maybe.withDefault ""
                in
                humanize s
                    |> String.uncons
                    |> Maybe.map (first >> String.fromChar)
                    |> Maybe.withDefault ""
                    |> Expect.equal (expected s)
        , fuzz (validWords []) "The tail of the string is lowercased" <|
            \s ->
                humanize s
                    |> String.uncons
                    |> Maybe.map second
                    |> Maybe.withDefault "a"
                    |> String.filter ((/=) ' ')
                    |> String.all Char.isLower
                    |> Expect.equal True
                    |> Expect.onFail "Not all characters in the string are lowercased"
        , fuzz Fuzz.string "It yields the same string after removing underscores, dashes and spaces" <|
            \s ->
                let
                    expected =
                        String.replace "-" ""
                            >> String.replace "_" ""
                            >> Regex.replace (regex "\\s+") (\_ -> "")
                            >> String.toUpper
                in
                humanize s
                    |> String.replace " " ""
                    |> String.toUpper
                    |> Expect.equal (expected s)
        , fuzz (validWords []) "It adds a space before each group of uppercase letter" <|
            \s ->
                let
                    expected =
                        Regex.replace (regex "[A-Z]+") (\{ match } -> " " ++ match)
                            >> String.toLower
                            >> String.trim
                in
                humanize s
                    |> String.toLower
                    |> Expect.equal (expected s)
        , fuzz Fuzz.string "It does not leave double spaces around" <|
            \s ->
                humanize s
                    |> String.contains "  "
                    |> Expect.equal False
                    |> Expect.onFail "The string contains double spaces"
        , fuzz idString "It strips the _id at the end" <|
            \s ->
                humanize s
                    |> String.endsWith "id"
                    |> Expect.equal False
                    |> Expect.onFail "The string should not end with id"
        ]


idString : Fuzzer String
idString =
    validWords [ '-', '_' ]
        |> Fuzz.map (\s -> s ++ "s_id")


validWords : List Char -> Fuzzer String
validWords ch =
    TestData.randomStringsWithChars ch


regex : String -> Regex.Regex
regex str =
    Maybe.withDefault Regex.never <|
        Regex.fromString str
