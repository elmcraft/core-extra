module String.DasherizeTest exposing (dasherizeTest)

import Char
import Expect
import Fuzz exposing (..)
import String exposing (replace)
import String.Extra exposing (..)
import String.TestData as TestData
import Test exposing (..)


dasherizeTest : Test
dasherizeTest =
    describe "dasherize"
        [ fuzz string "It is a lowercased string" <|
            \s ->
                dasherize s
                    |> String.toLower
                    |> Expect.equal (dasherize s)
        , fuzz string "It replaces spaces and underscores with a dash" <|
            \s ->
                let
                    expected =
                        String.toLower
                            >> String.trim
                            >> replace "  " " "
                            >> replace " " "-"
                            >> replace "\t" "-"
                            >> replace "\n" "-"
                            >> replace "_" "-"
                            >> replace "--" "-"
                            >> replace "--" "-"
                in
                dasherize (String.toLower s)
                    |> String.toLower
                    |> Expect.equal (expected s)
        ]
