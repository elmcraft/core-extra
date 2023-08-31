module BasicsTests exposing (suite)

import Basics.Extra
    exposing
        ( atLeast
        , atMost
        , curry
        , flip
        , fractionalModBy
        , inDegrees
        , inRadians
        , inTurns
        , isSafeInteger
        , maxSafeInteger
        , minSafeInteger
        , orderBy
        , safeDivide
        , safeIntegerDivide
        , safeModBy
        , safeRemainderBy
        , swap
        , toOrder
        , toOrderDesc
        , uncurry
        )
import Expect exposing (Expectation, FloatingPointTolerance(..))
import List.Extra as ListX
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Basics.Extra additional doc tests"
        [ swapTest
        , numbersTests
        , fractionalModByDocTests
        , inDegreesTest
        , inRadiansDocTests
        , inTurnsDocTests
        , higherOrderHelpersTests
        , orderByTests
        , toOrderTests
        , toOrderDescTests
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


fractionalModByDocTests : Test
fractionalModByDocTests =
    describe "fractionalModBy"
        [ test "example 1" <|
            \() -> fractionalModBy 2 4.5 |> expectAlmostEqual 0.5
        , test "example 2" <|
            \() -> fractionalModBy 2 -4.5 |> expectAlmostEqual 1.5
        , test "example 3" <|
            \() -> fractionalModBy -2 4.5 |> expectAlmostEqual -1.5
        ]


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


type Color
    = Red
    | Black
    | Blue


colorToComparable : Color -> Int
colorToComparable =
    \c ->
        case c of
            Red ->
                0

            Black ->
                1

            Blue ->
                2


type alias Car =
    { manufacturer : String
    , model : String
    , cylinders : Int
    , color : Color
    }


fordMustangEco : Car
fordMustangEco =
    { manufacturer = "Ford"
    , model = "Mustang EcoBoost"
    , cylinders = 4
    , color = Blue
    }


fordMustangShelby : Car
fordMustangShelby =
    { manufacturer = "Ford"
    , model = "Mustang Shelby GT350"
    , cylinders = 8
    , color = Red
    }


dodgeViper : Car
dodgeViper =
    { manufacturer = "Dodge"
    , model = "Viper ACR"
    , cylinders = 10
    , color = Black
    }


bmw340i : Car
bmw340i =
    { manufacturer = "BMW"
    , model = "340i"
    , cylinders = 6
    , color = Blue
    }


carPermutations : List (List Car)
carPermutations =
    ListX.permutations
        [ dodgeViper, fordMustangEco, bmw340i, fordMustangShelby ]


orderByTests : Test
orderByTests =
    let
        expectations : (Car -> Car -> Order) -> List (List Car -> Expectation)
        expectations order =
            List.map
                (\p ->
                    List.sortWith order p |> Expect.equalLists
                )
                carPermutations
    in
    describe "orderBy"
        [ test "order by manufacturer, model" <|
            \() ->
                let
                    sorted =
                        [ bmw340i, dodgeViper, fordMustangEco, fordMustangShelby ]

                    order : Car -> Car -> Order
                    order =
                        orderBy
                            [ toOrder .manufacturer
                            , toOrder .model
                            ]
                in
                Expect.all (expectations order) sorted
        , test "order by model, manufacturer" <|
            \() ->
                let
                    sorted =
                        [ bmw340i, fordMustangEco, fordMustangShelby, dodgeViper ]

                    order : Car -> Car -> Order
                    order =
                        orderBy
                            [ toOrder .model
                            , toOrder .manufacturer
                            ]
                in
                Expect.all (expectations order) sorted
        , test "order by color, cylinders, manufacturer, model" <|
            \() ->
                let
                    sorted =
                        [ fordMustangShelby, dodgeViper, fordMustangEco, bmw340i ]

                    order : Car -> Car -> Order
                    order =
                        orderBy
                            [ .color >> colorToComparable |> toOrder
                            , toOrder .cylinders
                            , toOrder .manufacturer
                            , toOrder .model
                            ]
                in
                Expect.all (expectations order) sorted
        , test "orderBy with an empty list should preserve the original ordering of the list" <|
            \() ->
                let
                    thunkedExpectations : List (() -> Expectation)
                    thunkedExpectations =
                        List.map
                            (\p ->
                                \() ->
                                    List.sortWith (orderBy []) p
                                        |> Expect.equalLists p
                            )
                            carPermutations
                in
                Expect.all thunkedExpectations ()
        ]


type alias ToOrderProduct =
    { field : Int }


type ToOrderSum
    = A
    | B


toOrderTests : Test
toOrderTests =
    describe "toOrder"
        [ test "use a type alias accessor to create an ordering function for that type alias" <|
            \() ->
                let
                    order : ToOrderProduct -> ToOrderProduct -> Order
                    order =
                        toOrder .field

                    unsorted : List ToOrderProduct
                    unsorted =
                        [ { field = 9 }
                        , { field = 8 }
                        , { field = 7 }
                        ]

                    sorted : List ToOrderProduct
                    sorted =
                        [ { field = 7 }
                        , { field = 8 }
                        , { field = 9 }
                        ]
                in
                List.sortWith order unsorted |> Expect.equalLists sorted
        , test "create a custom ordering function based on a sum type, and sort with it" <|
            \() ->
                let
                    order : ToOrderSum -> ToOrderSum -> Order
                    order a b =
                        case ( a, b ) of
                            ( A, A ) ->
                                EQ

                            ( B, B ) ->
                                EQ

                            ( A, _ ) ->
                                LT

                            ( B, _ ) ->
                                GT

                    unsorted : List ToOrderSum
                    unsorted =
                        [ B, A, B, A, B ]

                    sorted : List ToOrderSum
                    sorted =
                        [ A, A, B, B, B ]
                in
                List.sortWith order unsorted |> Expect.equalLists sorted
        ]


toOrderDescTests : Test
toOrderDescTests =
    describe "toOrderDesc"
        [ test "verify that sorting in descending order works as expected" <|
            \() ->
                List.sortWith (toOrderDesc identity) [ 1, 2, 1, 3, 4 ]
                    |> Expect.equalLists [ 4, 3, 2, 1, 1 ]
        ]
