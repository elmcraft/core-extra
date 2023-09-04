module String.DasherizeTest exposing (dasherizeTest)

import Char.Extra
import Expect
import Fuzz exposing (..)
import Regex exposing (Regex)
import String
import String.Extra exposing (dasherize)
import Test exposing (..)


dasherizeTest : Test
dasherizeTest =
    describe "dasherize"
        [ fuzz string "It is a lowercased string" <|
            \s ->
                dasherize s
                    |> String.toLower
                    |> Expect.equal (dasherize s)
        , fuzz string "It has no spaces in the resulting string" <|
            \s ->
                let
                    whiteSpaceChecker =
                        List.any Char.Extra.isSpace
                in
                dasherize (String.toLower s)
                    |> String.toList
                    |> whiteSpaceChecker
                    |> Expect.equal False
        , fuzz string "It has no consecutive dashes in the resulting string" <|
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
