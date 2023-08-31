module ResultTests exposing (all)

import Expect
import Fuzz
import Result.Extra
    exposing
        ( andMap
        , combine
        , combineBoth
        , combineFirst
        , combineMap
        , combineMapBoth
        , combineMapFirst
        , combineMapSecond
        , combineSecond
        , error
        , extract
        , filter
        , isErr
        , isOk
        , join
        , mapBoth
        , merge
        , or
        , orElse
        , orElseLazy
        , orLazy
        , partition
        , singleton
        , toTask
        , unwrap
        )
import Task
import Test exposing (Test, describe, fuzz, fuzz2, test)


all : Test
all =
    describe "Result.Extra"
        [ commonHelperTests
        , combiningTests
        , applyingTests
        , alternativesTests
        , toTaskTests
        ]


commonHelperTests : Test
commonHelperTests =
    describe "Common Helpers"
        [ test "isOk - Ok" <|
            \_ ->
                Expect.equal True (isOk <| Ok 2)
        , test "isOk - Err" <|
            \_ ->
                Expect.equal False (isOk <| Err 42)
        , test "isErr - Ok" <|
            \_ ->
                Expect.equal False (isErr <| Ok 2)
        , test "isErr - Err" <|
            \_ ->
                Expect.equal True (isErr <| Err 42)
        , test "extract - Ok" <|
            \_ ->
                Expect.equal (extract (always 42) <| Ok 2) 2
        , test "extract - Err" <|
            \_ ->
                Expect.equal (extract (\e -> e ++ " world") <| Err "hello") "hello world"
        , test "unwrap - Ok" <|
            \_ ->
                Expect.equal (unwrap "error" String.fromInt <| Ok 2) "2"
        , test "unwrap - Err" <|
            \_ ->
                Expect.equal (unwrap "error" String.fromInt <| Err 42) "error"
        , test "error - Ok" <|
            \_ ->
                Expect.equal (error <| Ok 2) Nothing
        , test "error - Err" <|
            \_ ->
                Expect.equal (error <| Err 42) <| Just 42
        , test "mapBoth - Ok" <|
            \_ ->
                Expect.equal (mapBoth String.fromFloat String.fromInt <| Ok 2)
                    (Ok "2")
        , test "mapBoth - Err" <|
            \_ ->
                Expect.equal (mapBoth String.fromFloat String.fromInt <| Err 4.2)
                    (Err "4.2")
        , test "merge - Ok" <|
            \_ ->
                Expect.equal (merge <| Ok 42) 42
        , test "merge - Err" <|
            \_ ->
                Expect.equal (merge <| Err 42) 42
        , test "join - Ok Ok" <|
            \_ ->
                Expect.equal (join <| Ok <| Ok 42) (Ok 42)
        , test "join - Ok Err" <|
            \_ ->
                Expect.equal (join <| Ok <| Err 42) (Err 42)
        , test "join - Err" <|
            \_ ->
                Expect.equal (join <| Err 42) (Err 42)
        , partitionTests
        , filterTests
        ]


partitionTests : Test
partitionTests =
    describe "partition"
        [ test "empty list" <|
            \_ ->
                Expect.equal (partition []) ( [], [] )
        , test "only Ok" <|
            \_ ->
                Expect.equal (partition [ Ok 99 ]) ( [ 99 ], [] )
        , test "only Err" <|
            \_ ->
                Expect.equal (partition [ Err 99 ]) ( [], [ 99 ] )
        , test "mixed list" <|
            \_ ->
                Expect.equal (partition [ Ok 99, Err "Nope", Ok -5 ]) ( [ 99, -5 ], [ "Nope" ] )
        ]


filterTests : Test
filterTests =
    describe "filter"
        [ test "err is ignored" <|
            \_ ->
                Err "previous error"
                    |> filter "is not 1" ((==) 1)
                    |> Expect.equal (Err "previous error")
        , test "ok passes filter" <|
            \_ ->
                Ok 1
                    |> filter "is not 1" ((==) 1)
                    |> Expect.equal (Ok 1)
        , test "ok filtered out" <|
            \() ->
                Ok 2
                    |> filter "is not 1" ((==) 1)
                    |> Expect.equal (Err "is not 1")
        ]


combiningTests : Test
combiningTests =
    describe "Combining"
        [ combineTests
        , combineMapTests
        , test "combineFirst - Ok" <|
            \_ -> Expect.equal (combineFirst ( Ok 42, 9001 )) <| Ok ( 42, 9001 )
        , test "combineFirst - Err" <|
            \_ -> Expect.equal (combineFirst ( Err 42, 9001 )) <| Err 42
        , test "combineSecond - Ok" <|
            \_ -> Expect.equal (combineSecond ( 9001, Ok 42 )) <| Ok ( 9001, 42 )
        , test "combineSecond - Err" <|
            \_ -> Expect.equal (combineSecond ( 9001, Err 42 )) <| Err 42
        , test "combineBoth - Ok Ok" <|
            \_ -> Expect.equal (combineBoth ( Ok 42, Ok 42 )) <| Ok ( 42, 42 )
        , test "combineBoth - Ok Err" <|
            \_ -> Expect.equal (combineBoth ( Ok 42, Err 9001 )) <| Err 9001
        , test "combineBoth - Err Ok" <|
            \_ -> Expect.equal (combineBoth ( Err 9001, Ok 42 )) <| Err 9001
        , test "combineBoth - Err Err" <|
            \_ -> Expect.equal (combineBoth ( Err 9001, Err 42 )) <| Err 9001
        , fuzz (Fuzz.result Fuzz.string Fuzz.int) "combineMapFirst identity == combineFirst" <|
            \r -> Expect.equal (combineFirst ( r, "42" )) (combineMapFirst identity ( r, "42" ))
        , fuzz (Fuzz.result Fuzz.string Fuzz.int) "combineMapSecond identity == combineSecond" <|
            \r -> Expect.equal (combineSecond ( "42", r )) (combineMapSecond identity ( "42", r ))
        , fuzz2 (Fuzz.result Fuzz.string Fuzz.int)
            (Fuzz.result Fuzz.string Fuzz.int)
            "combineMapBoth identity identity == combineBoth"
          <|
            \r1 r2 ->
                Expect.equal (combineMapBoth identity identity ( r1, r2 )) <| combineBoth ( r1, r2 )
        ]


combineTests : Test
combineTests =
    describe "combine"
        [ test "empty list" <|
            \_ ->
                Expect.equal (combine []) <| Ok []
        , test "all Ok" <|
            \_ ->
                Expect.equal (combine [ Ok 42, Ok 9001 ]) <| Ok [ 42, 9001 ]
        , test "all Err" <|
            \_ ->
                Expect.equal (combine [ Err 42, Err 9001 ]) <| Err 42
        , test "Err first" <|
            \_ ->
                Expect.equal (combine [ Err 42, Ok "hi" ]) <| Err 42
        , test "Err last" <|
            \_ ->
                Expect.equal (combine [ Ok "tada", Err 9001 ]) <| Err 9001
        , test "Err middle" <|
            \_ ->
                Expect.equal (combine [ Ok "hello", Err 42, Ok "world" ]) <| Err 42
        ]


combineMapTests : Test
combineMapTests =
    describe "combineMap"
        [ test "empty list" <|
            \_ ->
                Expect.equal (combineMap identity []) (Ok [])
        , test "all Ok" <|
            \_ ->
                Expect.equal (combineMap (always <| Ok 42) [ 1, 2, 3, 4 ]) <| Ok [ 42, 42, 42, 42 ]
        , test "all Ok #2" <|
            \_ ->
                Expect.equal (combineMap (\n -> Ok <| n + 1) [ 1, 2, 3, 4 ]) <| Ok [ 2, 3, 4, 5 ]
        , test "all Err" <|
            \_ ->
                Expect.equal (combineMap (always <| Err 42) [ 1, 2, 3, 4 ]) <| Err 42
        , test "mixed" <|
            \_ ->
                Expect.equal (combineMap identity [ Ok 9001, Err 42 ]) <| Err 42
        , fuzz2 (Fuzz.result Fuzz.string Fuzz.int)
            (Fuzz.result Fuzz.string Fuzz.int)
            "combineMap identity == combine"
          <|
            \r1 r2 ->
                Expect.equal (combineMap identity [ r1, r2 ]) (combine [ r1, r2 ])
        ]


applyingTests : Test
applyingTests =
    describe "Applying"
        [ test "singleton" <|
            \_ ->
                Expect.equal (singleton 42) (Ok 42)
        , andMapTests
        ]


andMapTests : Test
andMapTests =
    describe "andMap"
        [ test "Err -> Err -> Err" <|
            \_ ->
                Expect.equal (Err "Oh" |> andMap (Err "No!")) (Err "Oh")
        , test "Err -> Ok -> Err" <|
            \_ ->
                Expect.equal (Err "Oh" |> andMap (Ok 2)) (Err "Oh")
        , test "Ok -> Err -> Err" <|
            \_ ->
                Expect.equal (Ok ((+) 1) |> andMap (Err "No!")) (Err "No!")
        , test "Ok -> Ok -> Ok" <|
            \_ ->
                Expect.equal (Ok ((+) 1) |> andMap (Ok 2)) (Ok 3)
        ]


alternativesTests : Test
alternativesTests =
    describe "Alternatives"
        [ orTests
        , orLazyTests
        , orElseTests
        , orElseLazyTests
        ]


orTests : Test
orTests =
    describe "or"
        [ test "Ok1 -> Ok2 -> Ok1" <|
            \_ ->
                Expect.equal (or (Ok 2) (Ok 3)) <| Ok 2
        , test "Ok -> Err -> Ok" <|
            \_ ->
                Expect.equal (or (Ok 2) (Err "42")) <| Ok 2
        , test "Err -> Ok -> Ok" <|
            \_ ->
                Expect.equal (or (Err "42") (Ok 2)) <| Ok 2
        , test "Err1 -> Err2 -> Err2" <|
            \_ ->
                Expect.equal (or (Err "1") (Err "2")) <| Err "2"
        ]


orLazyTests : Test
orLazyTests =
    describe "orLazy"
        [ test "Ok1 -> Ok2 -> Ok1" <|
            \_ ->
                Expect.equal (orLazy (Ok 2) (always <| Ok 3)) <| Ok 2
        , test "Ok -> Err -> Ok" <|
            \_ ->
                Expect.equal (orLazy (Ok 2) (always <| Err "42")) <| Ok 2
        , test "Err -> Ok -> Ok" <|
            \_ ->
                Expect.equal (orLazy (Err "42") (always <| Ok 2)) <| Ok 2
        , test "Err1 -> Err2 -> Err2" <|
            \_ ->
                Expect.equal (orLazy (Err "1") (always <| Err "2")) <| Err "2"
        ]


orElseTests : Test
orElseTests =
    describe "orElse"
        [ test "Ok1 -> Ok2 -> Ok2" <|
            \_ ->
                Expect.equal (orElse (Ok 2) (Ok 3)) <| Ok 3
        , test "Ok -> Err -> Ok" <|
            \_ ->
                Expect.equal (orElse (Ok 2) (Err "42")) <| Ok 2
        , test "Err -> Ok -> Ok" <|
            \_ ->
                Expect.equal (orElse (Err "42") (Ok 2)) <| Ok 2
        , test "Err1 -> Err2 -> Err1" <|
            \_ ->
                Expect.equal (orElse (Err "1") (Err "2")) <| Err "1"
        ]


orElseLazyTests : Test
orElseLazyTests =
    describe "orElseLazy"
        [ test "Ok1 -> Ok2 -> Ok2" <|
            \_ ->
                Expect.equal (orElseLazy (always <| Ok 3) (Ok 2)) <| Ok 2
        , test "Ok -> Err -> Ok" <|
            \_ ->
                Expect.equal (orElseLazy (always <| Err "42") (Ok 2)) <| Ok 2
        , test "Err -> Ok -> Ok" <|
            \_ ->
                Expect.equal (orElseLazy (always <| Ok 2) (Err "42")) <| Ok 2
        , test "Err1 -> Err2 -> Err1" <|
            \_ ->
                Expect.equal (orElseLazy (always <| Err "2") (Err "1")) <| Err "2"
        ]


toTaskTests : Test
toTaskTests =
    describe "toTask"
        [ test "Ok" <|
            \_ ->
                Expect.equal (toTask (Ok 4)) (Task.succeed 4)
        , test "Err" <|
            \_ ->
                Expect.equal (toTask (Err "Oh")) (Task.fail "Oh")
        ]
