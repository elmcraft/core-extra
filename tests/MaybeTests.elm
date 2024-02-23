module MaybeTests exposing (suite)

import Array
import Expect
import Maybe.Extra
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Maybe.Extra"
        [ describe "orElse"
            [ test "both Just" <|
                \() ->
                    Just 4
                        |> Maybe.Extra.orElse (Just 5)
                        |> Expect.equal (Just 4)
            , test "pipe input Nothing" <|
                \() ->
                    Nothing
                        |> Maybe.Extra.orElse (Just 5)
                        |> Expect.equal (Just 5)
            , test "pipe function Nothing" <|
                \() ->
                    Just 4
                        |> Maybe.Extra.orElse Nothing
                        |> Expect.equal (Just 4)
            , test "both Nothing" <|
                \() ->
                    Nothing
                        |> Maybe.Extra.orElse Nothing
                        |> Expect.equal Nothing
            ]
        , describe "orLazy"
            [ test "both Just" <|
                \() ->
                    Maybe.Extra.orLazy (Just 4) (\() -> Just 5)
                        |> Expect.equal (Just 4)
            , test "first Nothing" <|
                \() ->
                    Maybe.Extra.orLazy Nothing (\() -> Just 5)
                        |> Expect.equal (Just 5)
            , test "second Nothing" <|
                \() ->
                    Maybe.Extra.orLazy (Just 4) (\() -> Nothing)
                        |> Expect.equal (Just 4)
            , test "both Nothing" <|
                \() ->
                    Maybe.Extra.orLazy Nothing (\() -> Nothing)
                        |> Expect.equal Nothing
            ]
        , describe "orElseLazy"
            [ test "both Just" <|
                \() ->
                    Just 4
                        |> Maybe.Extra.orElseLazy (\() -> Just 5)
                        |> Expect.equal (Just 4)
            , test "pipe input Nothing" <|
                \() ->
                    Nothing
                        |> Maybe.Extra.orElseLazy (\() -> Just 5)
                        |> Expect.equal (Just 5)
            , test "pipe function Nothing" <|
                \() ->
                    Just 4
                        |> Maybe.Extra.orElseLazy (\() -> Nothing)
                        |> Expect.equal (Just 4)
            , test "both Nothing" <|
                \() ->
                    Nothing
                        |> Maybe.Extra.orElseLazy (\() -> Nothing)
                        |> Expect.equal Nothing
            ]
        , describe "orListLazy"
            [ test "empty" <|
                \() ->
                    []
                        |> Maybe.Extra.orListLazy
                        |> Expect.equal Nothing
            , test "all nothing" <|
                \() ->
                    [ \() -> Nothing
                    , \() -> Nothing
                    , \() -> String.toInt ""
                    ]
                        |> Maybe.Extra.orListLazy
                        |> Expect.equal Nothing
            ]
        , describe "traverseArray"
            [ test "empty" <|
                \() ->
                    Array.empty
                        |> Maybe.Extra.combineMapArray (\x -> Just (x * 10))
                        |> Expect.equal (Just Array.empty)
            , test "all Just" <|
                \() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> Array.fromList
                        |> Maybe.Extra.combineMapArray (\x -> Just (x * 10))
                        |> Expect.equal (Just (Array.fromList [ 10, 20, 30, 40, 50 ]))
            , test "one Nothing fails the whole function" <|
                \() ->
                    [ [ 1 ], [ 2, 3 ], [] ]
                        |> Array.fromList
                        |> Maybe.Extra.combineMapArray List.head
                        |> Expect.equal Nothing
            ]
        , describe "combineArray"
            [ test "empty" <|
                \() ->
                    Array.empty
                        |> Maybe.Extra.combineArray
                        |> Expect.equal (Just Array.empty)
            , test "succeed" <|
                \() ->
                    [ Just 1, Just 2, Just 3 ]
                        |> Array.fromList
                        |> Maybe.Extra.combineArray
                        |> Expect.equal (Just (Array.fromList [ 1, 2, 3 ]))
            , test "fail" <|
                \() ->
                    [ Just 1, Nothing ]
                        |> Array.fromList
                        |> Maybe.Extra.combineArray
                        |> Expect.equal Nothing
            ]
        , describe "oneOf"
            [ test "empty" <|
                \() ->
                    Maybe.Extra.oneOf [] 0
                        |> Expect.equal Nothing
            , test "all fail" <|
                \() ->
                    Maybe.Extra.oneOf (List.repeat 10 (always Nothing)) 0
                        |> Expect.equal Nothing
            , test "last function succeeds" <|
                \() ->
                    Maybe.Extra.oneOf [ always Nothing, always Nothing, always Nothing, always (Just True) ] 0
                        |> Expect.equal (Just True)
            , test "first function succeeds" <|
                \() ->
                    0
                        |> Maybe.Extra.oneOf [ Just, Just << (+) 10, Just << (+) 20 ]
                        |> Expect.equal (Just 0)
            ]
        , describe "andThen3"
            [ test "returns a Just if it can" <|
                \() ->
                    Maybe.Extra.andThen3
                        (\a b c -> Just (a + b + c))
                        (Just 4)
                        (Just 2)
                        (Just 1)
                        |> Expect.equal (Just 7)
            ]
        , describe "andThen4"
            [ test "returns a Just if it can" <|
                \() ->
                    Maybe.Extra.andThen4
                        (\a b c d -> Just (a + b + c + d))
                        (Just 8)
                        (Just 4)
                        (Just 2)
                        (Just 1)
                        |> Expect.equal (Just 15)
            ]
        , describe "unwrap"
            [ test "returns the default value if it is a Nothing" <|
                \() ->
                    Maybe.Extra.unwrap 0 String.length Nothing
                        |> Expect.equal 0
            , test "returns the unwrapped value if it is a Just" <|
                \() ->
                    Maybe.Extra.unwrap 0 String.length (Just "abc")
                        |> Expect.equal 3
            ]
        , describe "unpack"
            [ test "returns the default value if it is a Nothing" <|
                \() ->
                    Maybe.Extra.unpack (\() -> 0) String.length Nothing
                        |> Expect.equal 0
            , test "returns the unpackped value if it is a Just" <|
                \() ->
                    Maybe.Extra.unpack (\() -> 0) String.length (Just "abc")
                        |> Expect.equal 3
            ]
        ]
