module BasicsTests exposing (suite)

import Basics.Extra
    exposing
        ( atLeast
        , atMost
        , curry
        , flip
        , inDegrees
        , inRadians
        , inTurns
        , isSafeInteger
        , maxSafeInteger
        , minSafeInteger
        , safeDivide
        , safeIntegerDivide
        , safeModBy
        , safeRemainderBy
        , swap
        , uncurry
        )
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Basics.Extra additional doc tests"
        [ swapTest
        , numbersTests
        , inDegreesTest
        , inRadiansDocTests
        , inTurnsDocTests
        , higherOrderHelpersTests
        ]


expectAlmostEqual : Float -> Float -> Expectation
expectAlmostEqual =
    Expect.within (Absolute 1.0e-20)


swapTest : Test
swapTest =
    test "swap swaps" <|
        \() ->
            swap ( 2, 0 ) |> swap |> Expect.equal ( 2, 0 )


numbersTests : Test
numbersTests =
    describe "numbers tests"
        [ test "maxSafeInteger behaves unexpectedly, as expected" <|
            \() ->
                maxSafeInteger + 1 |> Expect.equal (maxSafeInteger + 2)
        , test "minSafeInteger behaves unexpectedly, as expected" <|
            \() ->
                minSafeInteger - 1 |> Expect.equal (minSafeInteger - 2)
        , test "reports if an integer is within the range of safety" <|
            \() ->
                Expect.all
                    [ \() ->
                        isSafeInteger 5 |> Expect.equal True
                    , \() ->
                        isSafeInteger maxSafeInteger |> Expect.equal True
                    , \() ->
                        isSafeInteger minSafeInteger |> Expect.equal True
                    , \() ->
                        minSafeInteger - 1 |> isSafeInteger |> Expect.equal False
                    , \() ->
                        maxSafeInteger + 1 |> isSafeInteger |> Expect.equal False
                    ]
                    ()
        , test "defines an upper bound for a variable" <|
            \() ->
                Expect.all
                    [ \() ->
                        42 |> atMost 0 |> Expect.equal 0
                    , \() ->
                        -42 |> atMost 0 |> Expect.equal -42
                    ]
                    ()
        , test "defines a lower bound for a variable" <|
            \() ->
                Expect.all
                    [ \() ->
                        -42 |> atLeast 0 |> Expect.equal 0
                    , \() ->
                        42 |> atLeast 0 |> Expect.equal 42
                    ]
                    ()
        , test "divides a floating point number safely" <|
            \() ->
                Expect.all
                    [ \() ->
                        safeDivide 5 2 |> Expect.equal (Just 2.5)
                    , \() ->
                        safeDivide 5 0 |> Expect.equal Nothing
                    ]
                    ()
        , test "divides an integer safely" <|
            \() ->
                Expect.all
                    [ \() ->
                        safeIntegerDivide 5 2 |> Expect.equal (Just 2)
                    , \() ->
                        safeIntegerDivide 5 0 |> Expect.equal Nothing
                    ]
                    ()
        , test "all of the modular arithmetic and none of the crashing" <|
            \() ->
                Expect.all
                    [ \() ->
                        safeModBy 2 4 |> Expect.equal (Just 0)
                    , \() ->
                        safeModBy 2 5 |> Expect.equal (Just 1)
                    , \() ->
                        safeModBy 0 4 |> Expect.equal Nothing
                    ]
                    ()
        , test "safe remainders and none of the crashing" <|
            \() ->
                Expect.all
                    [ \() ->
                        safeRemainderBy 2 4 |> Expect.equal (Just 0)
                    , \() ->
                        safeRemainderBy 2 5 |> Expect.equal (Just 1)
                    , \() ->
                        safeRemainderBy 0 4 |> Expect.equal Nothing
                    ]
                    ()
        ]


inDegreesTest : Test
inDegreesTest =
    test "inDegrees should behave as expected" <|
        \() ->
            inDegrees pi |> Expect.equal 180


inRadiansDocTests : Test
inRadiansDocTests =
    describe "inRadians"
        [ test "example 1" <|
            \() -> inRadians (degrees 90) |> expectAlmostEqual (pi / 2)
        , test "example 2" <|
            \() -> inRadians (turns 1) |> expectAlmostEqual (2 * pi)
        ]


inTurnsDocTests : Test
inTurnsDocTests =
    describe "inTurns"
        [ test "example 1" <|
            \() -> inTurns (degrees 180) |> expectAlmostEqual 0.5
        , test "example 2" <|
            \() -> inTurns (3 * pi) |> expectAlmostEqual 1.5
        ]


higherOrderHelpersTests : Test
higherOrderHelpersTests =
    describe "higher-order helpers"
        [ test "flip" <|
            \() ->
                flip safeDivide 5 0 |> Expect.equal (Just 0)
        , test "curry" <|
            \() ->
                curry identity 2 4 |> Expect.equal ( 2, 4 )
        , test "uncurry" <|
            \() ->
                uncurry safeDivide ( 0, 5 ) |> Expect.equal (Just 0)
        ]
