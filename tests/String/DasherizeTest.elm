module String.DasherizeTest exposing (dasherizeTest)

import Char.Extra
import Expect
import Fuzz
import Regex exposing (Regex)
import String.Extra exposing (..)
import Test exposing (Test, describe, fuzz)


dasherizeTest : Test
dasherizeTest =
    describe "dasherize"
        [ fuzz Fuzz.string "It is a lowercased string" <|
            \s ->
                dasherize s
                    |> String.toLower
                    |> Expect.equal (dasherize s)
        , fuzz Fuzz.string "It has no spaces in the resulting string" <|
            \s ->
                let
                    whiteSpaceChecker =
                        List.any Char.Extra.isSpace
                in
                dasherize (String.toLower s)
                    |> String.toList
                    |> whiteSpaceChecker
                    |> Expect.equal False
        , fuzz Fuzz.string "It has no consecutive dashes in the resulting string" <|
            \s ->
                let
                    consecutiveDashesChecker =
                        Regex.contains consecutiveDashesRegex
                in
                dasherize (String.toLower s)
                    |> consecutiveDashesChecker
                    |> Expect.equal False
        ]


consecutiveDashesRegex : Regex
consecutiveDashesRegex =
    Regex.fromString "\\-{2,}"
        |> Maybe.withDefault Regex.never
