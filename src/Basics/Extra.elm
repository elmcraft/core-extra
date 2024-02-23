module Basics.Extra exposing
    ( maxSafeInteger, minSafeInteger, isSafeInteger
    , safeDivide, safeIntegerDivide
    , safeModBy, safeRemainderBy
    , inDegrees, inRadians, inTurns
    , flip, curry, uncurry
    )

{-| Additional basic functions.


# Numbers

@docs maxSafeInteger, minSafeInteger, isSafeInteger


# Math

@docs safeDivide, safeIntegerDivide
@docs safeModBy, safeRemainderBy


# Angles

@docs inDegrees, inRadians, inTurns


# Higher-Order Helpers

@docs flip, curry, uncurry

-}


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
