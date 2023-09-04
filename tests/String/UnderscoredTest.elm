module String.UnderscoredTest exposing (underscoredTest)

import Char.Extra
import Expect
import Fuzz exposing (..)
import Regex exposing (Regex)
import String
import String.Extra exposing (..)
import Test exposing (..)


underscoredTest : Test
underscoredTest =
    describe "underscored"
        [ fuzz string "It is a lowercased string" <|
            \s ->
                underscored s
                    |> String.toLower
                    |> Expect.equal (underscored s |> String.toLower)
        , fuzz string "It has no whitespace in the resulting string" <|
            \s ->
                let
                    whiteSpaceChecker =
                        List.any Char.Extra.isSpace
                in
                underscored (String.toLower s)
                    |> String.toList
                    |> whiteSpaceChecker
                    |> Expect.equal False
        , fuzz string "It has no consecutive underscores in the resulting string" <|
            \s ->
                let
                    consecutiveUnderscoresChecker =
                        Regex.contains consecutiveUnderscoresRegex
                in
                underscored (String.toLower s)
                    |> consecutiveUnderscoresChecker
                    |> Expect.equal False
        ]


consecutiveUnderscoresRegex : Regex
consecutiveUnderscoresRegex =
    Regex.fromString "_{2,}"
        |> Maybe.withDefault Regex.never
