module ArrayTests exposing (suite)

{-| Even though most implementations seem robust as they are now,
the tests are here to allow confident refactoring & changing.
-}

import Array exposing (Array)
import Array.Extra as Array
import Expect exposing (Expectation)
import Fuzz
import Random
import Test exposing (Test, test)


suite : Test
suite =
    Test.describe "Array.Extra"
        [ Test.describe "all"
            [ Test.describe "True"
                [ Test.fuzz
                    (Fuzz.array Fuzz.int)
                    "filter test |> all test"
                    (\array ->
                        array
                            |> Array.filter isEven
                            |> Array.all isEven
                            |> Expect.equal
                                True
                    )
                , test "example"
                    (\() ->
                        Array.fromList [ 2, 4 ]
                            |> Array.all isEven
                            |> Expect.equal
                                True
                    )
                ]
            , Test.describe "False"
                [ Test.fuzz
                    (Fuzz.constant
                        (\before after -> { before = before, after = after })
                        |> Fuzz.andMap (Fuzz.array Fuzz.int)
                        |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    )
                    "1 failing element included"
                    (\{ before, after } ->
                        Array.append
                            (before |> Array.push 1)
                            after
                            |> Array.all isEven
                            |> Expect.equal
                                False
                    )
                , test "example"
                    (\() ->
                        Array.fromList [ 2, 3 ]
                            |> Array.all isEven
                            |> Expect.equal
                                False
                    )
                ]
            ]
        , Test.describe "any"
            [ Test.describe "True"
                [ Test.fuzz
                    (Fuzz.constant
                        (\before after -> { before = before, after = after })
                        |> Fuzz.andMap (Fuzz.array Fuzz.int)
                        |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    )
                    "1 passing element included"
                    (\{ before, after } ->
                        Array.append
                            (before |> Array.push 2)
                            after
                            |> Array.any isEven
                            |> Expect.equal
                                True
                    )
                , test "example"
                    (\() ->
                        Array.fromList [ 1, 2 ]
                            |> Array.any isEven
                            |> Expect.equal
                                True
                    )
                ]
            , Test.describe "False"
                [ Test.fuzz
                    (Fuzz.array Fuzz.int)
                    "removeWhen test |> any test"
                    (\array ->
                        array
                            |> Array.removeWhen isEven
                            |> Array.any isEven
                            |> Expect.equal
                                False
                    )
                , test "example"
                    (\() ->
                        Array.fromList [ 1, 3 ]
                            |> Array.any isEven
                            |> Expect.equal
                                False
                    )
                ]
            ]
        , Test.describe "member"
            [ Test.fuzz
                (Fuzz.constant
                    (\before after ->
                        { before = before, after = after }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                )
                "included → True"
                (\{ before, after } ->
                    Array.append
                        (before |> Array.push 123456)
                        after
                        |> Array.member 123456
                        |> Expect.equal
                            True
                )
            , Test.fuzz
                (Fuzz.array Fuzz.int)
                "all removed → False"
                (\array ->
                    array
                        |> Array.removeWhen (\element -> element == 123456)
                        |> Array.member 123456
                        |> Expect.equal
                            False
                )
            ]
        , Test.describe "update"
            [ test "index valid"
                (\() ->
                    Array.fromList [ 1, 2, 3 ]
                        |> Array.update 1 (\n -> n + 10)
                        |> expectEqualArrays
                            (Array.fromList [ 1, 12, 3 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative"
                (\{ array, index } ->
                    array
                        |> Array.update index (\n -> n + 10)
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index too high"
                (\{ array, above } ->
                    array
                        |> Array.update ((array |> Array.length) + above) (\n -> n + 10)
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "pop"
            [ test "empty → Array.empty"
                (\() ->
                    Array.empty
                        |> Array.pop
                        |> expectEqualArrays
                            Array.empty
                )
            , Test.fuzz
                (Fuzz.constant
                    (\beforeLast last ->
                        { beforeLast = beforeLast, last = last }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap Fuzz.int
                )
                "push |> pop  → no change"
                (\{ beforeLast, last } ->
                    beforeLast
                        |> Array.push last
                        |> Array.pop
                        |> expectEqualArrays
                            beforeLast
                )
            ]
        , Test.describe "splitAt"
            [ test "index valid"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.splitAt 2
                        |> Expect.equal
                            ( Array.fromList [ 1, 2 ], Array.fromList [ 3, 4 ] )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt 0)
                )
                "index not positive"
                (\{ array, index } ->
                    array
                        |> Array.splitAt index
                        |> Expect.equal
                            ( Array.empty, array )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index too high"
                (\{ array, above } ->
                    array
                        |> Array.splitAt ((array |> Array.length) + above)
                        |> Expect.equal
                            ( array, Array.empty )
                )
            ]
        , Test.describe "removeAt"
            [ test "index valid"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.removeAt 2
                        |> expectEqualArrays
                            (Array.fromList [ 1, 2, 4 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative"
                (\{ array, index } ->
                    array
                        |> Array.removeAt index
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index too high"
                (\{ array, above } ->
                    array
                        |> Array.removeAt ((array |> Array.length) + above)
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "insertAt"
            [ test "index valid"
                (\() ->
                    Array.fromList [ 'a', 'c' ]
                        |> Array.insertAt 1 'b'
                        |> expectEqualArrays
                            (Array.fromList [ 'a', 'b', 'c' ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "index negative"
                (\{ array, index } ->
                    array
                        |> Array.insertAt index 12345
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 1 Random.maxInt)
                )
                "index too high"
                (\{ array, above } ->
                    array
                        |> Array.insertAt ((array |> Array.length) + above) 12345
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "sliceFrom"
            [ test "index positive valid"
                (\() ->
                    Array.fromList (List.range 0 6)
                        |> Array.sliceFrom 3
                        |> expectEqualArrays
                            (Array.fromList [ 3, 4, 5, 6 ])
                )
            , test "index negative valid"
                (\() ->
                    Array.fromList (List.range 0 6)
                        |> Array.sliceFrom -3
                        |> expectEqualArrays
                            (Array.fromList [ 4, 5, 6 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index positive too high"
                (\{ array, above } ->
                    array
                        |> Array.sliceFrom ((array |> Array.length) + above)
                        |> expectEqualArrays
                            Array.empty
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array below -> { array = array, below = below })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index negative too low"
                (\{ array, below } ->
                    array
                        |> Array.sliceFrom (-(array |> Array.length) - below)
                        |> expectEqualArrays
                            array
                )
            ]
        , Test.describe "sliceUntil"
            [ test "index positive valid"
                (\() ->
                    Array.fromList (List.range 0 6)
                        |> Array.sliceUntil 3
                        |> expectEqualArrays
                            (Array.fromList [ 0, 1, 2 ])
                )
            , test "index negative valid"
                (\() ->
                    Array.fromList (List.range 0 6)
                        |> Array.sliceUntil -3
                        |> expectEqualArrays
                            (Array.fromList [ 0, 1, 2, 3 ])
                )
            , Test.fuzz
                (Fuzz.array Fuzz.int)
                "index 0"
                (\array ->
                    array
                        |> Array.sliceUntil 0
                        |> expectEqualArrays
                            Array.empty
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array above -> { array = array, above = above })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index positive too high"
                (\{ array, above } ->
                    array
                        |> Array.sliceUntil ((array |> Array.length) + above)
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array below -> { array = array, below = below })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange 0 Random.maxInt)
                )
                "index negative too low"
                (\{ array, below } ->
                    array
                        |> Array.sliceUntil (-(array |> Array.length) - below)
                        |> expectEqualArrays
                            Array.empty
                )
            ]
        , Test.describe "filterMap"
            [ Test.fuzz
                (Fuzz.array Fuzz.int)
                "all Just"
                (\array ->
                    array
                        |> Array.map Just
                        |> Array.filterMap identity
                        |> expectEqualArrays
                            array
                )
            , Test.fuzz
                (Fuzz.array (Fuzz.constant Nothing))
                "all Nothing"
                (\arrayOfNothing ->
                    arrayOfNothing
                        |> Array.filterMap identity
                        |> expectEqualArrays
                            Array.empty
                )
            , test "some Nothing"
                (\() ->
                    Array.fromList [ Just 3, Nothing, Just 5, Nothing ]
                        |> Array.filterMap identity
                        |> expectEqualArrays
                            (Array.fromList [ 3, 5 ])
                )
            ]
        , Test.describe "apply"
            [ Test.fuzz
                (Fuzz.array Fuzz.int)
                "more elements than functions"
                (\after4 ->
                    Array.append (Array.repeat 4 100) after4
                        |> Array.apply
                            (Array.fromList
                                [ negate
                                , identity
                                , \n -> n + 10
                                , \_ -> 0
                                ]
                            )
                        |> expectEqualArrays
                            (Array.fromList [ -100, 100, 110, 0 ])
                )
            , Test.fuzz
                (Fuzz.array (Fuzz.constant (\_ -> 0)))
                "more functions than elements"
                (\after3 ->
                    Array.repeat 3 100
                        |> Array.apply
                            (Array.append
                                (Array.fromList [ negate, identity, \n -> n + 10 ])
                                after3
                            )
                        |> expectEqualArrays
                            (Array.fromList [ -100, 100, 110 ])
                )
            ]
        , Test.fuzz
            (Fuzz.array Fuzz.int)
            "mapToList"
            (\array ->
                array
                    |> Array.mapToList String.fromInt
                    |> Expect.equalLists
                        (array
                            |> Array.map String.fromInt
                            |> Array.toList
                        )
            )
        , Test.fuzz
            (Fuzz.array Fuzz.string)
            "indexedMapToList"
            (\array ->
                array
                    |> Array.indexedMapToList (\i el -> ( i, el ))
                    |> Expect.equalLists
                        (array
                            |> Array.toIndexedList
                        )
            )
        , Test.describe "map2"
            -- `zip` will probably always be implemented with `map2`.
            -- No need to test both
            [ Test.fuzz
                (Fuzz.constant
                    (\first second -> { first = first, second = second })
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                )
                "behaves like List.map2"
                (\{ first, second } ->
                    Array.map2 (\a b -> a + b)
                        (first |> Array.fromList)
                        (second |> Array.fromList)
                        |> expectEqualArrays
                            (List.map2 (\a b -> a + b) first second
                                |> Array.fromList
                            )
                )
            , test "first array shorter than the last example"
                (\() ->
                    Array.map2 Tuple.pair
                        (Array.fromList [ 1, 2, 3, 4 ])
                        (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        |> expectEqualArrays
                            (Array.fromList
                                [ ( 1, 'a' )
                                , ( 2, 'b' )
                                , ( 3, 'c' )
                                , ( 4, 'd' )
                                ]
                            )
                )
            , test "first array longer than the last example"
                (\() ->
                    Array.map2 Tuple.pair
                        (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        (Array.fromList [ 1, 2, 3, 4 ])
                        |> expectEqualArrays
                            (Array.fromList
                                [ ( 'a', 1 )
                                , ( 'b', 2 )
                                , ( 'c', 3 )
                                , ( 'd', 4 )
                                ]
                            )
                )
            ]
        , -- `zip3` will probably always be implemented using `map3`.
          -- No need to test both
          Test.describe "map3"
            [ Test.fuzz
                (Fuzz.constant
                    (\first second third ->
                        { first = first, second = second, third = third }
                    )
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                    |> Fuzz.andMap (Fuzz.list Fuzz.int)
                )
                "behaves like List.map3"
                (\{ first, second, third } ->
                    Array.map3 (\a b c -> a + b + c)
                        (first |> Array.fromList)
                        (second |> Array.fromList)
                        (third |> Array.fromList)
                        |> expectEqualArrays
                            (List.map3 (\a b c -> a + b + c) first second third
                                |> Array.fromList
                            )
                )
            , test "first array the shortest example"
                (\() ->
                    Array.map3 (\a b c -> ( a, b, c ))
                        (Array.fromList [ "a", "b", "c" ])
                        (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        (Array.fromList [ 1, 2, 3, 4 ])
                        |> expectEqualArrays
                            (Array.fromList
                                [ ( "a", 'a', 1 )
                                , ( "b", 'b', 2 )
                                , ( "c", 'c', 3 )
                                ]
                            )
                )
            , test "second array the shortest example"
                (\() ->
                    Array.map3 (\a b c -> ( a, b, c ))
                        (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        (Array.fromList [ "a", "b", "c" ])
                        (Array.fromList [ 1, 2, 3, 4 ])
                        |> expectEqualArrays
                            (Array.fromList
                                [ ( 'a', "a", 1 )
                                , ( 'b', "b", 2 )
                                , ( 'c', "c", 3 )
                                ]
                            )
                )
            , test "third array the shortest example"
                (\() ->
                    Array.map3 (\a b c -> ( a, b, c ))
                        (Array.fromList [ 'a', 'b', 'c', 'd', 'e' ])
                        (Array.fromList [ 1, 2, 3, 4 ])
                        (Array.fromList [ "a", "b", "c" ])
                        |> expectEqualArrays
                            (Array.fromList
                                [ ( 'a', 1, "a" )
                                , ( 'b', 2, "b" )
                                , ( 'c', 3, "c" )
                                ]
                            )
                )
            ]
        , Test.describe "removeWhen"
            [ test "example"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.removeWhen isEven
                        |> expectEqualArrays
                            (Array.fromList [ 1, 3 ])
                )
            , Test.fuzz
                (Fuzz.array Fuzz.int)
                "filter is |> removeWhen is  → Array.empty"
                (\array ->
                    array
                        |> Array.filter isEven
                        |> Array.removeWhen isEven
                        |> expectEqualArrays
                            Array.empty
                )
            , Test.fuzz
                (Fuzz.array Fuzz.int)
                "removeWhen is |> filter is  → Array.empty"
                (\array ->
                    array
                        |> Array.removeWhen isEven
                        |> Array.filter isEven
                        |> expectEqualArrays
                            Array.empty
                )
            ]
        , Test.describe "unzip"
            [ Test.fuzz
                (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char))
                "behaves like List.unzip"
                (\listOfTuple ->
                    listOfTuple
                        |> Array.fromList
                        |> Array.unzip
                        |> Expect.equal
                            (listOfTuple
                                |> List.unzip
                                |> Tuple.mapBoth Array.fromList Array.fromList
                            )
                )
            , Test.fuzz
                (Fuzz.constant (\first second -> { first = first, second = second })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.char)
                )
                "restores what was before zip with the same length taken"
                (\{ first, second } ->
                    let
                        minLength =
                            Basics.min (first |> Array.length) (second |> Array.length)
                    in
                    Array.zip first second
                        |> Array.unzip
                        |> Expect.equal
                            ( first |> Array.sliceUntil minLength
                            , second |> Array.sliceUntil minLength
                            )
                )
            , test "example"
                (\() ->
                    Array.fromList [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
                        |> Array.unzip
                        |> Expect.equal
                            ( Array.fromList [ 1, 2, 3 ]
                            , Array.fromList [ 'a', 'b', 'c' ]
                            )
                )
            ]
        , Test.describe "reverse"
            [ Test.fuzz
                (Fuzz.list Fuzz.int)
                "like List.reverse"
                (\list ->
                    list
                        |> Array.fromList
                        |> Array.reverse
                        |> expectEqualArrays
                            (list |> List.reverse |> Array.fromList)
                )
            , test "example"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.reverse
                        |> expectEqualArrays
                            (Array.fromList [ 4, 3, 2, 1 ])
                )
            ]
        , Test.describe "resizelRepeat"
            [ test "length less than current"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.resizelRepeat 3 0
                        |> expectEqualArrays
                            (Array.fromList [ 1, 2, 3 ])
                )
            , test "length greater than current"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.resizelRepeat 6 0
                        |> expectEqualArrays
                            (Array.fromList [ 1, 2, 3, 4, 0, 0 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array length -> { array = array, length = length })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt 0)
                )
                "length not positive  → Array.empty"
                (\{ array, length } ->
                    array
                        |> Array.resizelRepeat length 0
                        |> expectEqualArrays
                            Array.empty
                )
            ]
        , Test.describe "resizerRepeat"
            [ test "length less than current"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.resizerRepeat 3 0
                        |> expectEqualArrays
                            (Array.fromList [ 2, 3, 4 ])
                )
            , test "length greater than current"
                (\() ->
                    Array.fromList [ 1, 2, 3, 4 ]
                        |> Array.resizerRepeat 6 0
                        |> expectEqualArrays
                            (Array.fromList [ 0, 0, 1, 2, 3, 4 ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array length -> { array = array, length = length })
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt 0)
                )
                "length not positive  → Array.empty"
                (\{ array, length } ->
                    array
                        |> Array.resizelRepeat length 0
                        |> expectEqualArrays
                            Array.empty
                )
            ]
        , Test.describe "resizelIndexed"
            [ test "length less than current"
                (\() ->
                    Array.fromList [ "a", "b", "c" ]
                        |> Array.resizelIndexed 2 String.fromInt
                        |> expectEqualArrays
                            (Array.fromList [ "a", "b" ])
                )
            , test "length greater than current"
                (\() ->
                    Array.fromList [ "a", "b", "c" ]
                        |> Array.resizelIndexed 5 String.fromInt
                        |> expectEqualArrays
                            (Array.fromList [ "a", "b", "c", "3", "4" ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array length -> { array = array, length = length })
                    |> Fuzz.andMap (Fuzz.array Fuzz.string)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt 0)
                )
                "length not positive  → Array.empty"
                (\{ array, length } ->
                    array
                        |> Array.resizelIndexed length String.fromInt
                        |> expectEqualArrays
                            Array.empty
                )
            ]
        , Test.describe "resizerIndexed"
            [ test "length less than current"
                (\() ->
                    Array.fromList [ "a", "b", "c" ]
                        |> Array.resizerIndexed 2 String.fromInt
                        |> expectEqualArrays
                            (Array.fromList [ "b", "c" ])
                )
            , test "length greater than current"
                (\() ->
                    Array.fromList [ "a", "b", "c" ]
                        |> Array.resizerIndexed 5 String.fromInt
                        |> expectEqualArrays
                            (Array.fromList [ "0", "1", "a", "b", "c" ])
                )
            , Test.fuzz
                (Fuzz.constant
                    (\array index -> { array = array, index = index })
                    |> Fuzz.andMap (Fuzz.array Fuzz.string)
                    |> Fuzz.andMap (Fuzz.intRange Random.minInt -1)
                )
                "negative length  → Array.empty"
                (\{ array, index } ->
                    array
                        |> Array.resizerIndexed index String.fromInt
                        |> expectEqualArrays
                            Array.empty
                )
            ]
        , Test.describe "intersperse"
            [ Test.fuzz Fuzz.int
                "empty → Array.empty"
                (\separator ->
                    Array.empty
                        |> Array.intersperse separator
                        |> expectEqualArrays
                            Array.empty
                )
            , Test.fuzz
                (Fuzz.constant
                    (\onlyElement separator ->
                        { onlyElement = onlyElement, separator = separator }
                    )
                    |> Fuzz.andMap Fuzz.int
                    |> Fuzz.andMap Fuzz.int
                )
                "one → one"
                (\{ onlyElement, separator } ->
                    Array.empty
                        |> Array.push onlyElement
                        |> Array.intersperse separator
                        |> expectEqualArrays
                            (Array.empty
                                |> Array.push onlyElement
                            )
                )
            , Test.fuzz
                (Fuzz.constant
                    (\base separator ->
                        { base = base, separator = separator }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap Fuzz.int
                )
                "combined length"
                (\{ base, separator } ->
                    base
                        |> Array.intersperse separator
                        |> Array.length
                        |> Expect.equal
                            (((base |> Array.length) * 2 - 1)
                                |> max 0
                            )
                )
            , test "multiple"
                (\() ->
                    Array.fromList [ "turtles", "turtles", "turtles" ]
                        |> Array.intersperse "on"
                        |> expectEqualArrays
                            (Array.fromList
                                [ "turtles", "on", "turtles", "on", "turtles" ]
                            )
                )
            ]
        , Test.describe "interweave"
            [ Test.fuzz
                (Fuzz.constant
                    (\base toInterweave ->
                        { base = base, toInterweave = toInterweave }
                    )
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                    |> Fuzz.andMap (Fuzz.array Fuzz.int)
                )
                "lengths add up"
                (\{ base, toInterweave } ->
                    base
                        |> Array.interweave toInterweave
                        |> Array.length
                        |> Expect.equal
                            ((base |> Array.length)
                                + (toInterweave |> Array.length)
                            )
                )
            , test "less to interweave"
                (\() ->
                    Array.fromList [ "a0", "a1", "a2" ]
                        |> Array.interweave
                            (Array.fromList [ "b0" ])
                        |> expectEqualArrays
                            (Array.fromList
                                [ "a0", "b0", "a1", "a2" ]
                            )
                )
            , test "more to interweave"
                (\() ->
                    Array.fromList [ "a0", "a1", "a2" ]
                        |> Array.interweave
                            (Array.fromList [ "b0", "b1", "b2", "b3", "b4" ])
                        |> expectEqualArrays
                            (Array.fromList
                                [ "a0", "b0", "a1", "b1", "a2", "b2", "b3", "b4" ]
                            )
                )
            ]
        ]


isEven : Int -> Bool
isEven =
    \int -> (int |> modBy 2) == 0


expectEqualArrays : Array a -> Array a -> Expectation
expectEqualArrays expected actual =
    Expect.equal
        (expected |> Array.toList)
        (actual |> Array.toList)
