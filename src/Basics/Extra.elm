module Basics.Extra exposing
    ( swap
    , maxSafeInteger, minSafeInteger, isSafeInteger
    , atMost, atLeast
    , safeDivide, safeIntegerDivide
    , safeModBy, safeRemainderBy, fractionalModBy
    , inDegrees, inRadians, inTurns
    , flip, curry, uncurry
    , orderBy, toOrder, toOrderDesc
    )

{-| Additional basic functions.


# Tuples

@docs swap


# Numbers

@docs maxSafeInteger, minSafeInteger, isSafeInteger


# Math

@docs atMost, atLeast
@docs safeDivide, safeIntegerDivide
@docs safeModBy, safeRemainderBy, fractionalModBy


# Angles

@docs inDegrees, inRadians, inTurns


# Higher-Order Helpers

@docs flip, curry, uncurry


# Comparison & Ordering

@docs orderBy, toOrder, toOrderDesc

-}


{-| Swaps the elements in a pair.

    swap ( 1, 2 ) --> ( 2, 1 )

-}
swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


{-| The maximum _safe_ value for an integer, defined as `2^53 - 1`. Anything
larger than that and behaviour becomes mathematically unsound.

    maxSafeInteger + 1 --> maxSafeInteger + 2

-}
maxSafeInteger : number
maxSafeInteger =
    2 ^ 53 - 1


{-| The minimum _safe_ value for an integer, defined as `-(2^53 - 1)`. Anything
smaller than that, and behaviour becomes mathematically unsound.

    minSafeInteger - 1 --> minSafeInteger - 2

-}
minSafeInteger : number
minSafeInteger =
    -maxSafeInteger


{-| Checks if a given integer is within the safe range, meaning it is between
`-(2^53 - 1)` and `2^53 - 1`.

    isSafeInteger 5 --> True

    isSafeInteger maxSafeInteger --> True

    isSafeInteger (maxSafeInteger + 1) --> False

-}
isSafeInteger : Int -> Bool
isSafeInteger number =
    minSafeInteger <= number && maxSafeInteger >= number


{-| Defines an upper bound for a variable.

    42 |> atMost 0 --> 0

    -42 |> atMost 0 --> -42

-}
atMost : comparable -> comparable -> comparable
atMost =
    min


{-| Defines a lower bound for a variable.

    -42 |> atLeast 0 --> 0

    42 |> atLeast 0 --> 42

-}
atLeast : comparable -> comparable -> comparable
atLeast =
    max


{-| Perform floating-point division (like Elm's `/` operator) that will never
crash the app. If the `y` argument in `safeDivide x y` is zero, we return `Nothing`.

    safeDivide 5 2 --> Just 2.5

    -- the interesting part
    safeDivide 5 0 --> Nothing

-}
safeDivide : Float -> Float -> Maybe Float
safeDivide x y =
    if y == 0 then
        Nothing

    else
        Just (x / y)


{-| Perform integer division (like Elm's `//` operator) that will never crash
the app. If the `y` argument in `safeIntegerDivide x y` is zero, we return `Nothing`.

    safeIntegerDivide 5 2 --> Just 2

    -- the interesting part
    safeIntegerDivide 5 0 --> Nothing

-}
safeIntegerDivide : Int -> Int -> Maybe Int
safeIntegerDivide x y =
    if y == 0 then
        Nothing

    else
        Just (x // y)


{-| Perform [modular arithmetic][ma] that will never crash the app. If the `modulus`
argument in `safeModBy modulus x` is zero, we return `Nothing`.

    safeModBy 2 4 --> Just 0

    safeModBy 2 5 --> Just 1

    -- the interesting part
    safeModBy 0 4 --> Nothing

Use [`safeRemainderBy`](#safeRemainderBy) for a different treatment of negative
numbers, or read Daan Leijen’s [Division and Modulus for Computer Scientists][dm]
for more information.

[ma]: https://en.wikipedia.org/wiki/Modular_arithmetic
[dm]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf

-}
safeModBy : Int -> Int -> Maybe Int
safeModBy modulus x =
    if modulus == 0 then
        Nothing

    else
        Just (modBy modulus x)


{-| Get the remainder after division in a way that will never crash the app. If
the `divisor` argument in `safeRemainderBy divisor x` is zero, we return `Nothing`.

    safeRemainderBy 2 4 --> Just 0

    safeRemainderBy 2 5 --> Just 1

    -- the interesting part
    safeRemainderBy 0 4 --> Nothing

Use [`safeModBy`](#safeModBy) for a different treatment of negative
numbers, or read Daan Leijen’s [Division and Modulus for Computer Scientists][dm]
for more information.

[dm]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf

-}
safeRemainderBy : Int -> Int -> Maybe Int
safeRemainderBy divisor x =
    if divisor == 0 then
        Nothing

    else
        Just (remainderBy divisor x)


{-| Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic)
involving floating point numbers.

The sign of the result is the same as the sign of the `modulus`
in `fractionalModBy modulus x`.

    fractionalModBy 2.5 5 --> 0

    fractionalModBy 2 4.5 == 0.5

    fractionalModBy 2 -4.5 == 1.5

    fractionalModBy -2 4.5 == -1.5

-}
fractionalModBy : Float -> Float -> Float
fractionalModBy modulus x =
    x - modulus * toFloat (floor (x / modulus))


{-| Convert standard Elm angles (radians) to degrees.

    inDegrees (turns 2) --> 720

    inDegrees pi --> 180

-}
inDegrees : Float -> Float
inDegrees angle =
    angle / degrees 1


{-| Convert standard Elm angles (radians) to radians.

    inRadians (degrees 90) == pi / 2

    inRadians (turns 1) == 2 * pi

-}
inRadians : Float -> Float
inRadians =
    identity


{-| Convert standard Elm angles (radians) to turns. One turn is equal to 360°.

    inTurns (degrees 180) == 0.5

    inTurns (3 * pi) == 1.5

-}
inTurns : Float -> Float
inTurns angle =
    angle / turns 1


{-| Flip the order of the first two arguments to a function.
-}
flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


{-| Change how arguments are passed to a function.
This splits paired arguments into two separate arguments.
-}
curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


{-| Change how arguments are passed to a function.
This combines two arguments into a single pair.
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


{-| Create an ordering function that can be used to sort
lists by multiple dimensions, by flattening multiple ordering functions into one.

This is equivalent to `ORDER BY` in SQL. The ordering function will order
its inputs based on the order that they appear in the `List (a -> a -> Order)` argument.

    type alias Pen =
        { model : String
        , tipWidthInMillimeters : Float
        }

    pens : List Pen
    pens =
        [ Pen "Pilot Hi-Tec-C Gel" 0.4
        , Pen "Morning Glory Pro Mach" 0.38
        , Pen "Pilot Hi-Tec-C Coleto" 0.5
        ]

    order : Pen -> Pen -> Order
    order =
        orderBy [ toOrder .tipWidthInMillimeters, toOrder .model ]

    List.sortWith order pens
    --> [ Pen "Morning Glory Pro Mach" 0.38
    --> , Pen "Pilot Hi-Tec-C Gel" 0.4
    --> , Pen "Pilot Hi-Tec-C Coleto" 0.5
    --> ]

If our `Pen` type alias above was represented a row in a database table, our `order` function as defined above would be equivalent
to this SQL clause:

    ORDER BY tipWidthInMillimeters, model

-}
orderBy : List (a -> a -> Order) -> (a -> a -> Order)
orderBy comparators a b =
    case comparators of
        [] ->
            EQ

        comparator :: rest ->
            case comparator a b of
                EQ ->
                    orderBy rest a b

                other ->
                    other


{-| Helper for multi-dimensional sort.

Takes a function that extracts a comparable value from a type `a` as a key,
and returns a function `a -> a -> Order`.

This is primarily a helper function for the `orderBy` function above.

    {- Simple example: wrapping a function that turns
       a custom type into an instance of `comparable`
    -}

    type Color
        = Red
        | Yellow
        | Green

    colorToComparable : Color -> Int
    colorToComparable light =
        case light of
            Red -> 0
            Yellow -> 1
            Green -> 2

    colorToOrder : Color -> Color -> Order
    colorToOrder =
        toOrder colorToComparable

    List.sortWith
        colorToOrder
        [ Yellow, Yellow, Red, Green, Red ]
    --> [ Red, Red, Yellow, Yellow, Green ]


    {- More interesting example: using the property accessor
       methods on a custom type with `toOrder`; we only need
       this function when we want to combine multiple ordering functions into one.
    -}

    type alias Light =
        { color : Color
        , action : String
        , timeActivatedSeconds : Float
        }

    lights : List Light
    lights =
        [ Light Green "Go" 60
        , Light Yellow "Slow down" 5.5
        , Light Red "Stop" 60
        ]

    List.sortWith
        ( orderBy
            [ toOrder .timeActivatedSeconds
            , toOrder (.color >> colorToComparable)
            ]
        )
        lights
    --> [ Light Yellow "Slow down" 5.5
    --> , Light Red "Stop" 60
    --> , Light Green "Go" 60
    --> ]

(Note that `List.sortWith colorOrder` above is identical to `List.sortBy colorToComparable`.)

-}
toOrder : (a -> comparable) -> (a -> a -> Order)
toOrder selector a b =
    Basics.compare (selector a) (selector b)


{-| Same as `toOrder`, with flipped comparisons to enable "sort by descending".

    type Color
        = Red
        | Yellow
        | Green

    colorToComparable : Color -> Int
    colorToComparable light =
        case light of
            Red -> 0
            Yellow -> 1
            Green -> 2

    colorToOrder : Color -> Color -> Order
    colorToOrder =
        toOrderDesc colorToComparable

    List.sortWith
        colorToOrder
        [ Yellow, Yellow, Red, Green, Red ]
    --> [ Green, Yellow, Yellow, Red, Red ]

-}
toOrderDesc : (a -> comparable) -> (a -> a -> Order)
toOrderDesc selector a b =
    Basics.compare (selector b) (selector a)
