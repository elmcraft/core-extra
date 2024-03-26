module Float.Extra exposing
    ( aboutEqual, equalWithin
    , toFixedDecimalPlaces, toFixedSignificantDigits, boundaryValuesAsUnicode
    , range
    , modBy
    , interpolateFrom
    )

{-| Convenience functions for dealing with Floats.


# Equality

@docs aboutEqual, equalWithin


# Formatting Floats

@docs toFixedDecimalPlaces, toFixedSignificantDigits, boundaryValuesAsUnicode


# Ranges

@docs range


# Modular arithmetic

@docs modBy


# Interpolation

@docs interpolateFrom

-}

-- toFixedDecimalDigits implementation


zeroes : Float -> String
zeroes v =
    String.repeat (floor v) "0"


adjustDecimalPlace : Float -> Float -> Float
adjustDecimalPlace x magnitude =
    if magnitude < 0 then
        x * 10 ^ -magnitude

    else
        x / 10 ^ magnitude


{-| As specified in <https://tc39.es/ecma402/#sec-torawprecision>
-}
toRawPrecision : Float -> Float -> String
toRawPrecision x precision =
    let
        ( significantBaseString, decimalPointIndex ) =
            toSignificantBase precision x
    in
    if decimalPointIndex >= precision then
        significantBaseString ++ zeroes (decimalPointIndex - precision + 1)

    else if decimalPointIndex == precision - 1 then
        significantBaseString

    else
        let
            candidateString =
                if decimalPointIndex >= 0 then
                    String.slice 0 (floor (decimalPointIndex + 1)) significantBaseString ++ "." ++ String.dropLeft (floor (decimalPointIndex + 1)) significantBaseString

                else
                    "0." ++ zeroes -(decimalPointIndex + 1) ++ significantBaseString
        in
        if String.contains "." candidateString && precision > minPrecision then
            cutUnnecessaryDecimalZeroes (round (precision - minPrecision)) candidateString

        else
            candidateString


minPrecision : Float
minPrecision =
    1


toSignificantBase : Float -> Float -> ( String, Float )
toSignificantBase precision x =
    if x == 0 then
        ( zeroes precision, 0 )

    else
        case String.split "e" (String.fromFloat x) of
            [ xToStringMantissa, xToStringExponent ] ->
                case String.toFloat xToStringExponent of
                    Just xExponent ->
                        toSignificantBaseScientific precision xToStringMantissa xToStringExponent xExponent

                    _ ->
                        toSignificantBaseNonScientific precision x

            _ ->
                toSignificantBaseNonScientific precision x


toSignificantBaseScientific : Float -> String -> String -> Float -> ( String, Float )
toSignificantBaseScientific precision xToStringMantissa xToStringExponent xExponent =
    let
        xToStringMantissaWithoutDecimalPoint =
            String.replace "." "" xToStringMantissa

        len =
            toFloat (String.length xToStringMantissaWithoutDecimalPoint)
    in
    if len <= precision then
        ( xToStringMantissaWithoutDecimalPoint ++ zeroes (precision - len), xExponent )

    else
        ( xToStringMantissa
            |> String.toFloat
            |> Maybe.map (\v -> toRawPrecision v precision ++ "e" ++ xToStringExponent)
            |> Maybe.withDefault xToStringMantissaWithoutDecimalPoint
        , precision - 1
        )


toSignificantBaseNonScientific : Float -> Float -> ( String, Float )
toSignificantBaseNonScientific precision x =
    let
        e_ =
            toFloat <| floor <| logBase 10 <| abs x

        decimalPlaceOffset =
            e_ - precision + 1

        n =
            round (adjustDecimalPlace x decimalPlaceOffset)
    in
    if adjustDecimalPlace (toFloat n) (precision - 1) >= 10 then
        ( String.fromInt (n // 10), e_ + 1 )

    else
        ( String.fromInt n, e_ )


cutUnnecessaryDecimalZeroes : Int -> String -> String
cutUnnecessaryDecimalZeroes num value =
    value
        |> String.toList
        |> List.reverse
        |> cutUnnecessaryDecimalZeroesHelp num
        |> List.reverse
        |> String.fromList


cutUnnecessaryDecimalZeroesHelp : Int -> List Char -> List Char
cutUnnecessaryDecimalZeroesHelp toCut lst =
    if toCut > 0 then
        case lst of
            '0' :: rst ->
                cutUnnecessaryDecimalZeroesHelp (toCut - 1) rst

            '.' :: rst ->
                rst

            _ ->
                lst

    else
        case lst of
            '.' :: tail ->
                tail

            _ ->
                lst


sign : Float -> String
sign x =
    if x >= 0 then
        ""

    else
        "-"


{-| Fix a float value represented as a string to a certain number of significant digits.

    Float.Extra.toFixedSignificantDigits 2 1.435 --> "1.4"

    Float.Extra.toFixedSignificantDigits 2 545435 --> "550000"

    Float.Extra.toFixedSignificantDigits 2 0.0039 --> "0.0039"

-}
toFixedSignificantDigits : Int -> Float -> String
toFixedSignificantDigits significantDigits value =
    if isNaN value then
        "NaN"

    else if isInfinite value then
        sign value ++ "Infinity"

    else
        sign value ++ toRawPrecision (abs value) (toFloat (max 1 significantDigits))



-- toFixedDecimalPlaces implementation


{-| Fix a float value represented to a certain number of decimal places as a string.

    Float.Extra.toFixedDecimalPlaces 3 0.0326232 --> "0.033"

-}
toFixedDecimalPlaces : Int -> Float -> String
toFixedDecimalPlaces decimalPlaces value =
    let
        padString s =
            case String.split "." s of
                [ v1, v2 ] ->
                    v1 ++ "." ++ String.padRight decimalPlaces '0' v2

                [ v1 ] ->
                    v1 ++ "." ++ String.repeat decimalPlaces "0"

                _ ->
                    s
    in
    if isNaN value then
        "NaN"

    else if isInfinite value then
        sign value ++ "Infinity"

    else if decimalPlaces <= 0 then
        roundAsFloat decimalPlaces value
            |> String.fromFloat

    else
        value
            |> roundToDecimal decimalPlaces
            |> String.fromFloat
            |> padString


roundAsFloat : Int -> Float -> Float
roundAsFloat places strNum =
    if places < 0 then
        strNum

    else
        roundToDecimal places strNum


roundToDecimal : Int -> Float -> Float
roundToDecimal places value =
    if places < 0 then
        value

    else
        let
            exp : Float
            exp =
                10.0 ^ toFloat places

            multiplyByExp : Float -> Float
            multiplyByExp =
                (*) exp

            divByExp : Float -> Float
            divByExp v =
                v / exp
        in
        value
            |> multiplyByExp
            |> round
            |> toFloat
            |> divByExp



-- boundaryValuesAsUnicode


{-| When showing Float values to users, we generally don't particularly want them to see programmer-y values like
`NaN` or `Infinity`. This function wraps a number formatting routine, but replaces those values with unicode symbols:

    format : Float -> String
    format =
        Float.Extra.toFixedSignificantDigits 3
            |> Float.Extra.boundaryValuesAsUnicode

    format (0 / 0) --> "∅"
    format (1 / 0) --> "∞"
    format (-1 / 0) --> "-∞"
    format (1 / 3) -> "0.333"

Of course using this is unsuitable for when you want the numbers to be machine readable.

-}
boundaryValuesAsUnicode : (Float -> String) -> Float -> String
boundaryValuesAsUnicode formatter value =
    if isNaN value then
        "∅"

    else if isInfinite value then
        sign value ++ "∞"

    else
        formatter value



-- Equality


{-| Comparing Floats with `==` is usually wrong, unless you basically care for reference equality, since floating point
numbers often have small precision drift.

    0.1 + 0.2 == 0.3 --> False

This function implements an approximation where we are asking - are these values close enough that we can consider their difference to be
due to floating point drift rather than a result of meaningful difference in calculation?

    (0.1 + 0.2) |> Float.Extra.aboutEqual 0.3 --> True

Note: this is unlikely to be appropriate if you are performing computations much smaller than one.

    (0.00001 + 0.00002) |> Float.Extra.aboutEqual 0.00003 --> True

This value handles Infinity and NaN like so:

    (1 / 0) |> Float.Extra.aboutEqual (100 / 0) --> True

    (0 / 0) |> Float.Extra.aboutEqual (0 / 0) --> False

-}
aboutEqual : Float -> Float -> Bool
aboutEqual a b =
    if isInfinite a || isInfinite b then
        a == b

    else
        abs (a - b) <= 1.0e-5 + 1.0e-8 * abs a


{-| Check if two values are equal within a given tolerance.

    Float.Extra.equalWithin 1.0e-6 1.9999 2.0001 --> False

    Float.Extra.equalWithin 1.0e-3 1.9999 2.0001 --> True

-}
equalWithin : Float -> Float -> Float -> Bool
equalWithin tolerance firstValue secondValue =
    abs (secondValue - firstValue) <= tolerance



-- Range


{-| Returns a List containing an arithmetic progression, similar to the Python
built-in range.

Takes a `start`, `stop` and `step` argument. The stop value is exclusive; it is not
included in the result. If `step` is positive, the last element is the largest
`start + i * step` less than `stop`; if `step` is negative, the last element is
the smallest `start + i * step` greater than `stop`. If the returned list would
contain an infinite number of values, an empty range is returned.

The arguments are not required to be whole numbers; however, the results are more
predictable if they are.

Differences from [List.range from the standard library](https://package.elm-lang.org/packages/elm/core/latest/List#range):

  - `List.range` is inclusive, meaning that the stop value will be included in the result
  - `List.range` supports `Int`, whereas this uses `Float`
  - `List.range` supports only increasing intervals (i.e. `List.range 3 1 == []` vs. `range 3 1 -1 == [3, 2]`)
  - `List.range` doesn't allow for specifying the step value

-}
range : Float -> Float -> Float -> List Float
range start stop step =
    if step == 0 then
        []

    else
        let
            n =
                (stop - start)
                    / step
                    |> ceiling
                    |> max 0

            helper i list =
                if i >= 0 then
                    helper (i - 1) (start + step * toFloat i :: list)

                else
                    list
        in
        helper (n - 1) []


{-| Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic)
involving floating point numbers.

The sign of the result is the same as the sign of the `modulus`
in `Float.Extra.modBy modulus x`.

    Float.Extra.modBy 2.5 5 --> 0

    Float.Extra.modBy 2 4.5 == 0.5

    Float.Extra.modBy 2 -4.5 == 1.5

    Float.Extra.modBy -2 4.5 == -1.5

-}
modBy : Float -> Float -> Float
modBy modulus x =
    x - modulus * toFloat (floor (x / modulus))


{-| Interpolate from the first value to the second, based on a parameter that
ranges from zero to one. Passing a parameter value of zero will return the start
value and passing a parameter value of one will return the end value.

    Float.Extra.interpolateFrom 5 10 0 --> 5

    Float.Extra.interpolateFrom 5 10 1 --> 10

    Float.Extra.interpolateFrom 5 10 0.6 --> 8

The end value can be less than the start value:

    Float.Extra.interpolateFrom 10 5 0.1 --> 9.5

Parameter values less than zero or greater than one can be used to extrapolate:

    Float.Extra.interpolateFrom 5 10 1.5 --> 12.5

    Float.Extra.interpolateFrom 5 10 -0.5 --> 2.5

    Float.Extra.interpolateFrom 10 5 -0.2 --> 11

-}
interpolateFrom : Float -> Float -> Float -> Float
interpolateFrom start end parameter =
    if parameter <= 0.5 then
        start + parameter * (end - start)

    else
        end + (1 - parameter) * (start - end)
