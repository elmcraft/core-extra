module CharTests exposing (suite)

import Char.Extra
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "The Char.Extra module"
        [ describe "isControl"
            [ test "Tab" <|
                \_ ->
                    Char.Extra.isControl '\t'
                        |> Expect.equal True
                        |> Expect.onFail "Tab is a control character"
            , test "a" <|
                \_ ->
                    Char.Extra.isControl 'a'
                        |> Expect.equal False
                        |> Expect.onFail "'a' is not a control character"
            ]
        , describe "isSpace"
            [ test "Tab" <|
                \_ ->
                    Char.Extra.isSpace '\t'
                        |> Expect.equal True
                        |> Expect.onFail "Tab is a whitespace character"
            , test "a" <|
                \_ ->
                    Char.Extra.isSpace 'a'
                        |> Expect.equal False
                        |> Expect.onFail "'a' is not a control character"
            , test "Space" <|
                \_ ->
                    Char.Extra.isSpace ' '
                        |> Expect.equal True
                        |> Expect.onFail "Space is a whitespace character"
            ]
        ]
