module String.Tests exposing (breakTest, cleanTest, countOccurrencesTest, decapitalizeTest, ellipsisTest, insertAtTest, isBlankTest, leftOfBackTest, nonBlankTest, pluralizeTest, rightOfBackTest, softBreakTest, surroundTest, toSentenceCaseTest, toTitleCaseTest, unquoteTest, wrapTest)

import Expect
import Fuzz exposing (Fuzzer)
import String exposing (fromChar, replace, toLower, toUpper, uncons)
import String.Extra exposing (..)
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Tuple exposing (first, second)


tail : String -> String
tail =
    uncons >> Maybe.map second >> Maybe.withDefault ""


toSentenceCaseTest : Test
toSentenceCaseTest =
    describe "toSentenceCase"
        [ fuzz Fuzz.string "It converts the first char of the string to uppercase" <|
            \string ->
                let
                    result =
                        string
                            |> toSentenceCase
                            |> uncons
                            |> Maybe.map (first >> fromChar)
                            |> Maybe.withDefault ""

                    expected =
                        string
                            |> uncons
                            |> Maybe.map (first >> fromChar >> toUpper)
                            |> Maybe.withDefault ""
                in
                Expect.equal expected result
        , fuzz Fuzz.string "The tail of the string remains untouched" <|
            \string ->
                let
                    result =
                        (toSentenceCase >> tail) string

                    expected =
                        tail string
                in
                Expect.equal expected result
        ]


decapitalizeTest : Test
decapitalizeTest =
    describe "decapitalize"
        [ fuzz Fuzz.string "It only converst the first char in the string to lowercase" <|
            \string ->
                let
                    result =
                        string
                            |> decapitalize
                            |> uncons
                            |> Maybe.map (first >> fromChar)
                            |> Maybe.withDefault ""

                    expected =
                        string
                            |> uncons
                            |> Maybe.map (first >> fromChar >> toLower)
                            |> Maybe.withDefault ""
                in
                Expect.equal expected result
        , fuzz Fuzz.string "It does not change the tail of the string" <|
            \string ->
                let
                    result =
                        (decapitalize >> tail) string

                    expected =
                        tail string
                in
                Expect.equal expected result
        ]


toTitleCaseTest : Test
toTitleCaseTest =
    describe "toTitleCase"
        [ fuzz (Fuzz.list Fuzz.string) "It converts the first letter of each word to uppercase" <|
            \strings ->
                let
                    result =
                        strings
                            |> String.join " "
                            |> toTitleCase
                            |> String.words

                    expected =
                        strings
                            |> String.join " "
                            |> String.words
                            |> List.map toSentenceCase
                in
                Expect.equal expected result
        , fuzz (Fuzz.list Fuzz.string) "It does not change the length of the string" <|
            \strings ->
                let
                    result =
                        strings
                            |> String.join " "
                            |> toTitleCase
                            |> String.length

                    expected =
                        strings
                            |> String.join " "
                            |> String.length
                in
                Expect.equal expected result
        ]


breakTest : Test
breakTest =
    describe "break"
        [ fuzz2 Fuzz.string (Fuzz.intRange 0 100) "The list should have as many elements as the ceil division of the length" <|
            \string width ->
                case ( string, width ) of
                    ( "", _ ) ->
                        break width string
                            |> List.length
                            |> Expect.equal 1

                    ( _, 0 ) ->
                        break width string
                            |> List.length
                            |> Expect.equal 1

                    _ ->
                        break width string
                            |> List.length
                            |> Expect.equal (ceiling <| (toFloat << String.length) string / toFloat width)
        , fuzz2 Fuzz.string (Fuzz.intRange 1 10) "Concatenating the result yields the original string" <|
            \string width ->
                break width string
                    |> String.concat
                    |> Expect.equal string
        , fuzz2 Fuzz.string (Fuzz.intRange 1 10) "No element in the list should have more than `width` chars" <|
            \string width ->
                break width string
                    |> List.map String.length
                    |> List.filter ((<) width)
                    |> List.isEmpty
                    |> Expect.equal True
                    |> Expect.onFail "The list has some long elements"
        ]


softBreakTest : Test
softBreakTest =
    describe "softBreak"
        [ fuzz2 Fuzz.string (Fuzz.intRange 1 10) "Concatenating the result yields the original string" <|
            \string width ->
                softBreak width (String.trim string)
                    |> String.concat
                    |> Expect.equal (String.trim string)
        , fuzz2 Fuzz.string (Fuzz.intRange 1 10) "The list should not have more elements than words" <|
            \string width ->
                softBreak width string
                    |> List.length
                    |> Expect.atMost (String.words string |> List.length)
        ]


cleanTest : Test
cleanTest =
    describe "clean"
        [ {- Test.skip <|
                 fuzz Fuzz.string "The String.split result is the same as String.words" <|
                     \string ->
                         let
                             result =
                                 string
                                     |> clean
                                     |> String.split " "

                             expected =
                                 String.words string
                         in
                         Expect.equal expected result
             ,
          -}
          fuzz Fuzz.string "It trims the string on the left side" <|
            \string ->
                string
                    |> clean
                    |> String.startsWith " "
                    |> Expect.equal False
                    |> Expect.onFail "Did not trim the start of the string"
        , fuzz Fuzz.string "It trims the string on the right side" <|
            \string ->
                string
                    |> clean
                    |> String.endsWith " "
                    |> Expect.equal False
                    |> Expect.onFail "Did not trim the end of the string"
        ]


insertAtTest : Test
insertAtTest =
    describe "insertAt"
        [ fuzz insertAtProducer "Result contains the substitution string" <|
            \( sub, at, string ) ->
                string
                    |> insertAt sub at
                    |> String.contains sub
                    |> Expect.equal True
                    |> Expect.onFail "Could not find substitution string in result"
        , fuzz insertAtProducer "Resulting string has length as the sum of both arguments" <|
            \( sub, at, string ) ->
                insertAt sub at string
                    |> String.length
                    |> Expect.equal (String.length sub + String.length string)
        , fuzz insertAtProducer "Start of the string remains the same" <|
            \( sub, at, string ) ->
                insertAt sub at string
                    |> String.slice 0 at
                    |> Expect.equal (String.slice 0 at string)
        , fuzz insertAtProducer "End of the string remains the same" <|
            \( sub, at, string ) ->
                insertAt sub at string
                    |> String.slice (at + String.length sub) (String.length string + String.length sub)
                    |> Expect.equal (String.slice at (String.length string) string)
        ]


insertAtProducer : Fuzzer ( String, Int, String )
insertAtProducer =
    Fuzz.map3 (\a b s -> ( "b" ++ s, b, String.repeat (a + b) "a" ))
        (Fuzz.intRange 0 10)
        (Fuzz.intRange 1 10)
        Fuzz.string


isBlankTest : Test
isBlankTest =
    describe "isBlank"
        [ test "Returns true if the given string is blank" <|
            \_ ->
                isBlank ""
                    |> Expect.equal True
                    |> Expect.onFail "Did not return true"
        , test "Returns false if the given string is not blank" <|
            \_ ->
                isBlank " Slartibartfast"
                    |> Expect.equal False
                    |> Expect.onFail "Did not return false"
        ]


nonBlankTest : Test
nonBlankTest =
    describe "nonBlank"
        [ test "Returns Nothing if the given string is blank" <|
            \_ ->
                nonBlank ""
                    |> Expect.equal Nothing
        , test "Returns just the string if the given string is not blank" <|
            \_ ->
                nonBlank " Slartibartfast"
                    |> Expect.equal (Just " Slartibartfast")
        ]


surroundTest : Test
surroundTest =
    describe "surround"
        [ fuzz2 Fuzz.string Fuzz.string "It starts with the wrapping string" <|
            \string wrap ->
                string
                    |> surround wrap
                    |> String.startsWith wrap
                    |> Expect.equal True
                    |> Expect.onFail "Did not start with the wrapping string"
        , fuzz2 Fuzz.string Fuzz.string "It ends with the wrapping string" <|
            \string wrap ->
                string
                    |> surround wrap
                    |> String.endsWith wrap
                    |> Expect.equal True
                    |> Expect.onFail "Did not end with the wrapping string"
        , fuzz2 Fuzz.string Fuzz.string "It contains the original string" <|
            \string wrap ->
                string
                    |> surround wrap
                    |> String.contains string
                    |> Expect.equal True
                    |> Expect.onFail "Did not contain the string"
        , fuzz2 Fuzz.string Fuzz.string "It does not have anything else inside" <|
            \string wrap ->
                let
                    result =
                        String.length (surround wrap string)

                    expected =
                        String.length string + (2 * String.length wrap)
                in
                Expect.equal expected result
        ]



-- TODO: ensure this test only gets strings that contain the needle in the
-- haystack?


countOccurrencesTest : Test
countOccurrencesTest =
    describe "countOccurrences"
        [ fuzz2 Fuzz.string Fuzz.string "Removing the occurences should yield the right length" <|
            \needle haystack ->
                let
                    times =
                        countOccurrences needle haystack

                    result =
                        String.length haystack - (times * String.length needle)

                    expected =
                        String.length (replace needle "" haystack)
                in
                Expect.equal expected result
        ]


ellipsisTest : Test
ellipsisTest =
    describe "ellipsis"
        [ fuzz2 (Fuzz.intRange 3 20) Fuzz.string "The resulting string length does not exceed the specified length" <|
            \howLong string ->
                ellipsis howLong string
                    |> String.length
                    |> (>=) howLong
                    |> Expect.equal True
                    |> Expect.onFail "Resulting string exceeds specified length"
        , fuzz2 (Fuzz.intRange 3 20) Fuzz.string "The resulting string contains three dots at the end if necessary" <|
            \howLong string ->
                let
                    result =
                        ellipsis howLong string
                in
                result
                    |> String.endsWith "..."
                    |> (if String.length string > howLong then
                            Expect.equal True >> Expect.onFail "Should add ellipsis to this string"

                        else
                            Expect.equal False >> Expect.onFail "Should not add ellipsis"
                       )
        , fuzz2 (Fuzz.intRange 3 20) Fuzz.string "It starts with the left of the original string" <|
            \howLong string ->
                let
                    result =
                        ellipsis howLong string

                    resultLeft =
                        String.dropRight 3 result
                in
                string
                    |> String.startsWith resultLeft
                    |> Expect.equal True
                    |> Expect.onFail "Should start with the original left"
        ]


unquoteTest : Test
unquoteTest =
    describe "unquote"
        [ test "Removes quotes from the start of the string" <|
            \_ ->
                unquote "\"Magrathea\""
                    |> Expect.equal "Magrathea"
        ]


wrapTest : Test
wrapTest =
    describe "wrap"
        [ fuzz2 (Fuzz.intRange 1 20) Fuzz.string "Wraps given string at the requested length" <|
            \howLong string ->
                wrap howLong string
                    |> String.split "\n"
                    |> List.map (\str -> String.length str <= howLong)
                    |> List.all ((==) True)
                    |> Expect.equal True
                    |> Expect.onFail "Given string was not wrapped at requested length"
        , test "Does not wrap string shorter than the requested length" <|
            \_ ->
                wrap 50 "Heart of Gold"
                    |> String.contains "\n"
                    |> Expect.equal False
                    |> Expect.onFail "Short string was wrapped"
        ]


pluralizeTest : Test
pluralizeTest =
    describe "pluralize"
        [ test "It uses the singular version when the count is one" <|
            \() ->
                pluralize "elf" "elves" 1
                    |> Expect.equal "1 elf"
        , test "It uses the plural version for > 1 count" <|
            \() ->
                pluralize "elf" "elves" 4
                    |> Expect.equal "4 elves"
        , test "It uses the plural version for 0 count" <|
            \() ->
                pluralize "elf" "elves" 0
                    |> Expect.equal "0 elves"
        ]


leftOfBackTest : Test
leftOfBackTest =
    test "leftOfBack" <|
        \() ->
            leftOfBack "_" "This_is_a_test_string"
                |> Expect.equal "This_is_a_test"


rightOfBackTest : Test
rightOfBackTest =
    test "rightOfBack" <|
        \() ->
            rightOfBack "_" "This_is_a_test_string"
                |> Expect.equal "string"
