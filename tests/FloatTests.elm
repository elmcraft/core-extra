module FloatTests exposing (modByTests, testAboutEqual, testBoundaryValuesAsUnicode, testRange, testToFixedDecimalPlaces, testToFixedSignificantDigits)

import Expect exposing (FloatingPointTolerance(..))
import Float.Extra exposing (aboutEqual)
import Fuzz exposing (Fuzzer)
import List.Extra exposing (Step(..))
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Utils exposing (expectAll)


testToFixedDecimalPlaces : Test
testToFixedDecimalPlaces =
    describe "String.toFixedDecimalPlaces should take a string representation of a float value and round it to a fixed number of decimal places."
        [ test "Should pad with zeroes to required decimal places" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 56
                    |> Expect.equal "56.00"
        , test "Should pad zero values to required decimal places" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 3 0
                    |> Expect.equal "0.000"
        , test "Should round to integer with 0 decimal places" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 0 4.92
                    |> Expect.equal "5"
        , test "Should remove decimal places and round up" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 123.345
                    |> Expect.equal "123.35"
        , test "Should pad numbers less than 1 to required decimal places" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 0.6
                    |> Expect.equal "0.60"
        , test "Should maintain decimal places if no rounding is required" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 0.06
                    |> Expect.equal "0.06"
        , test "Should remove decimal places and round for numbers between 0 and 1" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 0.006
                    |> Expect.equal "0.01"
        , test "Should round to 0 with padding for numbers between 0 and 1 that round down" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 0.003
                    |> Expect.equal "0.00"
        , test "handles NaN" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 (0 / 0)
                    |> Expect.equal "NaN"
        , test "handles Infinity" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 (1 / 0)
                    |> Expect.equal "Infinity"
        , test "handles -Infinity" <|
            \_ ->
                Float.Extra.toFixedDecimalPlaces 2 (-1 / 0)
                    |> Expect.equal "-Infinity"
        ]


testToFixedSignificantDigits : Test
testToFixedSignificantDigits =
    describe "String.toFixedSignificantDigits should take a string representation of a float value and round it to a fixed number of significant digits."
        [ test "should round down to required digits" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits 2 56.1
                    |> Expect.equal "56"
        , test "should round up to required digits" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits 2 56.8
                    |> Expect.equal "57"
        , test "handles negative values" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits 2 -56.8
                    |> Expect.equal "-57"
        , fuzz2 Fuzz.niceFloat (Fuzz.intRange 1 20) "produces at most n significant digits" <|
            \num digits ->
                Float.Extra.toFixedSignificantDigits digits num
                    |> String.toList
                    |> List.Extra.stoppableFoldl
                        (\d total ->
                            if d == '1' || d == '2' || d == '3' || d == '4' || d == '5' || d == '6' || d == '7' || d == '8' || d == '9' then
                                Continue (total + 1)

                            else if d == '0' || d == '.' || d == '-' then
                                Continue total

                            else if d == 'e' then
                                Stop total

                            else
                                Stop 999999999
                        )
                        0
                    |> Expect.atMost digits
        , fuzz2 Fuzz.niceFloat (Fuzz.intRange 1 20) "more or less preserves the value" <|
            \num digits ->
                num
                    |> Float.Extra.toFixedSignificantDigits digits
                    |> String.toFloat
                    |> Maybe.map (Expect.within (Expect.Relative (1 / toFloat digits)) num)
                    |> Maybe.withDefault (Expect.fail "Converting output to float failed")
        , test "negative digits just means 1" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits -1 4.15
                    |> Expect.equal "4"
        , test "tricky rounding" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits 3 80.99999999999967
                    |> Expect.equal "81"
        , test "handles very small numbers" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits 3 9.208633855450898e-12
                    |> Expect.equal "9.21e-12"
        , test "handles NaN" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits 2 (0 / 0)
                    |> Expect.equal "NaN"
        , test "handles Infinity" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits 2 (1 / 0)
                    |> Expect.equal "Infinity"
        , test "handles -Infinity" <|
            \_ ->
                Float.Extra.toFixedSignificantDigits 2 (-1 / 0)
                    |> Expect.equal "-Infinity"
        ]


testBoundaryValuesAsUnicode : Test
testBoundaryValuesAsUnicode =
    describe "boundaryValuesAsUnicode fixes infinity and NaN to unicode"
        [ test "handles NaN" <|
            \_ ->
                (0 / 0)
                    |> Float.Extra.boundaryValuesAsUnicode (Float.Extra.toFixedSignificantDigits 2)
                    |> Expect.equal "∅"
        , test "handles Infinity" <|
            \_ ->
                (1 / 0)
                    |> Float.Extra.boundaryValuesAsUnicode (Float.Extra.toFixedSignificantDigits 2)
                    |> Expect.equal "∞"
        , test "handles -Infinity" <|
            \_ ->
                (-1 / 0)
                    |> Float.Extra.boundaryValuesAsUnicode (Float.Extra.toFixedSignificantDigits 2)
                    |> Expect.equal "-∞"
        , fuzz Fuzz.niceFloat "works like formatter" <|
            \v ->
                v
                    |> Float.Extra.boundaryValuesAsUnicode (Float.Extra.toFixedSignificantDigits 2)
                    |> Expect.equal (Float.Extra.toFixedSignificantDigits 2 v)
        ]


testAboutEqual : Test
testAboutEqual =
    fuzz Fuzz.niceFloat "makes numbers about equal even after some operations" <|
        \a ->
            ((a + 10 + a - 10 - a) * 2 / 2)
                |> aboutEqual a
                |> Expect.equal True


testRange : Test
testRange =
    describe "range start stop step"
        [ test "returns [start, start + step, start + 2 * step, … stop - step]" <|
            \() ->
                expectAll
                    [ Float.Extra.range 0 5 1
                        |> Expect.equal [ 0, 1, 2, 3, 4 ]
                    , Float.Extra.range 0 5 2
                        |> Expect.equal [ 0, 2, 4 ]
                    , Float.Extra.range 2 5 2
                        |> Expect.equal [ 2, 4 ]
                    , Float.Extra.range -1 3 2
                        |> Expect.equal [ -1, 1 ]
                    ]
        , test "allows a negative step" <|
            \() ->
                expectAll
                    [ Float.Extra.range 5 0 -1
                        |> Expect.equal [ 5, 4, 3, 2, 1 ]
                    , Float.Extra.range 5 0 -2
                        |> Expect.equal [ 5, 3, 1 ]
                    , Float.Extra.range 5 2 -2
                        |> Expect.equal [ 5, 3 ]
                    , Float.Extra.range 3 -1 -2
                        |> Expect.equal [ 3, 1 ]
                    ]
        , test "returns an empty array if start >= stop and step > 0" <|
            \() ->
                expectAll
                    [ Float.Extra.range 5 5 2
                        |> Expect.equal []
                    , Float.Extra.range 6 5 2
                        |> Expect.equal []
                    , Float.Extra.range 10 10 1
                        |> Expect.equal []
                    , Float.Extra.range 10 10 0.5
                        |> Expect.equal []
                    , Float.Extra.range 0 0 1
                        |> Expect.equal []
                    , Float.Extra.range 0 0 0.5
                        |> Expect.equal []
                    , Float.Extra.range 20 10 2
                        |> Expect.equal []
                    , Float.Extra.range 20 10 1
                        |> Expect.equal []
                    , Float.Extra.range 20 10 0.5
                        |> Expect.equal []
                    ]
        , test "returns an empty array if start <= stop and step < 0" <|
            \() ->
                expectAll
                    [ Float.Extra.range 5 5 -2
                        |> Expect.equal []
                    , Float.Extra.range 5 6 -2
                        |> Expect.equal []
                    , Float.Extra.range 10 10 -1
                        |> Expect.equal []
                    , Float.Extra.range 10 10 -0.5
                        |> Expect.equal []
                    , Float.Extra.range 0 0 -1
                        |> Expect.equal []
                    , Float.Extra.range 0 0 -0.5
                        |> Expect.equal []
                    , Float.Extra.range 10 20 -2
                        |> Expect.equal []
                    , Float.Extra.range 10 20 -1
                        |> Expect.equal []
                    , Float.Extra.range 10 20 -0.5
                        |> Expect.equal []
                    ]
        , test "returns an empty array if step is zero" <|
            \() ->
                Float.Extra.range 0 5 0
                    |> Expect.equal []
        , fuzz2 (Fuzz.intRange -1000 1000) (Fuzz.intRange -1000 1000) "behaves the same as List.range for ints" <|
            \a b ->
                Float.Extra.range (toFloat (min a b)) (toFloat (max a b)) 1
                    |> List.map round
                    -- account for List.range being inclusive
                    |> (\y -> List.append y [ max a b ])
                    |> Expect.equal (List.range (min a b) (max a b))
        , test "returns exactly [start + step * i, …] for fractional steps" <|
            \() ->
                expectAll
                    [ Float.Extra.range 0 0.5 0.1
                        |> Expect.equal [ 0, 0.1, 0.1 * 2, 0.1 * 3, 0.1 * 4 ]
                    , Float.Extra.range 0.5 0 -0.1
                        |> Expect.equal [ 0.5, 0.5 - 0.1, 0.5 - 0.1 * 2, 0.5 - 0.1 * 3, 0.5 - 0.1 * 4 ]
                    , Float.Extra.range -2 -1.2 0.1
                        |> Expect.equal [ -2, -2 + 0.1, -2 + 0.1 * 2, -2 + 0.1 * 3, -2 + 0.1 * 4, -2 + 0.1 * 5, -2 + 0.1 * 6, -2 + 0.1 * 7 ]
                    , Float.Extra.range -1.2 -2 -0.1
                        |> Expect.equal [ -1.2, -1.2 - 0.1, -1.2 - 0.1 * 2, -1.2 - 0.1 * 3, -1.2 - 0.1 * 4, -1.2 - 0.1 * 5, -1.2 - 0.1 * 6, -1.2 - 0.1 * 7 ]
                    ]
        , test "returns exactly [start + step * i, …] for very small fractional steps" <|
            \() ->
                expectAll
                    [ Float.Extra.range 2.1e-31 5.0e-31 1.1e-31
                        |> Expect.equal [ 2.1e-31, 2.1e-31 + 1.1e-31, 2.1e-31 + 1.1e-31 * 2 ]
                    , Float.Extra.range 5.0e-31 2.1e-31 -1.1e-31
                        |> Expect.equal [ 5.0e-31, 5.0e-31 - 1.1e-31, 5.0e-31 - 1.1e-31 * 2 ]
                    ]
        , test "returns exactly [start + step * i, …] for very large fractional steps" <|
            \() ->
                expectAll
                    [ Float.Extra.range 1.0e300 2.0e300 3.0e299
                        |> Expect.equal [ 1.0e300, 1.0e300 + 3.0e299, 1.0e300 + 3.0e299 * 2, 1.0e300 + 3.0e299 * 3 ]
                    , Float.Extra.range 2.0e300 1.0e300 -3.0e299
                        |> Expect.equal [ 2.0e300, 2.0e300 - 3.0e299, 2.0e300 - 3.0e299 * 2, 2.0e300 - 3.0e299 * 3 ]
                    ]
        , fuzz fuzzRangeArgs "First element is always start" <|
            \( start, end, step ) ->
                case Float.Extra.range start end step |> List.head of
                    Just v ->
                        Expect.within (Expect.AbsoluteOrRelative 1.0e-8 1.0e-5) start v

                    Nothing ->
                        Expect.pass
        ]


fuzzRangeArgs : Fuzzer ( Float, Float, Float )
fuzzRangeArgs =
    -- Float.Extra.range can generate some REALLY big lists very easily
    -- this fuzzer makes sure that it generates inputs that lead to
    -- ranges with about 10 elements
    Fuzz.map4
        (\start step count extra ->
            ( start, start + (toFloat count + extra) * step, step )
        )
        Fuzz.float
        Fuzz.float
        (Fuzz.intRange -10 10)
        (Fuzz.floatRange 0 1)


modByTests : Test
modByTests =
    describe "modBy"
        [ test "example 1" <|
            \() -> Float.Extra.modBy 2 4.5 |> Expect.within (Absolute 1.0e-20) 0.5
        , test "example 2" <|
            \() -> Float.Extra.modBy 2 -4.5 |> Expect.within (Absolute 1.0e-20) 1.5
        , test "example 3" <|
            \() -> Float.Extra.modBy -2 4.5 |> Expect.within (Absolute 1.0e-20) -1.5
        , fuzz2 (Fuzz.filter (\x -> x /= 0) Fuzz.int) Fuzz.int "behaves like modBy for int-like values" <|
            \a b ->
                Float.Extra.modBy (toFloat a) (toFloat b)
                    |> Expect.within (Absolute 1.0e-20) (toFloat (modBy a b))
        ]
