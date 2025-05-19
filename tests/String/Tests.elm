module String.Tests exposing (breakTest, cleanTest, countOccurrencesTest, dasherizeTest, decapitalizeTest, ellipsisTest, insertAtTest, isBlankTest, leftOfBackTest, leftOfTest, nonBlankTest, pluralizeTest, rightOfBackTest, rightOfTest, softBreakTest, stripTagsTest, surroundTest, toSentenceCaseTest, toTitleCaseTest, underscoredTest, unquoteTest, wrapTest)

import Char.Extra
import Expect
import Fuzz exposing (Fuzzer, string)
import Regex exposing (Regex)
import String exposing (fromChar, replace, toLower, uncons)
import String.Extra
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Tuple exposing (first, second)


tail : String -> String
tail =
    uncons >> Maybe.map second >> Maybe.withDefault ""


stripTagsTest : Test
stripTagsTest =
    fuzz Fuzz.string "removes anything that could look like HTML" <|
        \str ->
            str
                |> String.Extra.stripTags
                |> Regex.contains (regexFromString "<\\/?[^>]+>")
                |> Expect.equal False


toSentenceCaseTest : Test
toSentenceCaseTest =
    describe "toSentenceCase"
        [ fuzz Fuzz.string "It converts the first char of the string to uppercase" <|
            \string ->
                string
                    |> String.Extra.toSentenceCase
                    |> String.left 1
                    |> (\pref -> String.startsWith pref (String.toUpper string))
                    |> Expect.equal True
        , fuzz Fuzz.string "The tail of the string remains untouched" <|
            \string ->
                string
                    |> String.Extra.toSentenceCase
                    |> String.endsWith (tail string)
                    |> Expect.equal True
        ]


decapitalizeTest : Test
decapitalizeTest =
    describe "decapitalize"
        [ fuzz Fuzz.string "It only converst the first char in the string to lowercase" <|
            \string ->
                let
                    result =
                        string
                            |> String.Extra.decapitalize
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
                        (String.Extra.decapitalize >> tail) string

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
                            |> String.Extra.toTitleCase
                            |> String.words

                    expected =
                        strings
                            |> String.join " "
                            |> String.words
                            |> List.map String.Extra.toSentenceCase
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
                        String.Extra.break width string
                            |> List.length
                            |> Expect.equal 1

                    ( _, 0 ) ->
                        String.Extra.break width string
                            |> List.length
                            |> Expect.equal 1

                    _ ->
                        String.Extra.break width string
                            |> List.length
                            |> Expect.equal (ceiling <| (toFloat << String.length) string / toFloat width)
        , fuzz2 Fuzz.string (Fuzz.intRange 1 10) "Concatenating the result yields the original string" <|
            \string width ->
                String.Extra.break width string
                    |> String.concat
                    |> Expect.equal string
        , fuzz2 Fuzz.string (Fuzz.intRange 1 10) "No element in the list should have more than `width` chars" <|
            \string width ->
                String.Extra.break width string
                    |> List.map String.length
                    |> List.filter (\x -> width < x)
                    |> List.isEmpty
                    |> Expect.equal True
                    |> Expect.onFail "The list has some long elements"
        ]


softBreakTest : Test
softBreakTest =
    describe "softBreak"
        [ fuzz2 Fuzz.string (Fuzz.intRange 1 10) "Concatenating the result yields the original string" <|
            \string width ->
                String.Extra.softBreak width (String.trim string)
                    |> String.concat
                    |> Expect.equal (String.trim string)
        , fuzz2 (Fuzz.map String.Extra.clean Fuzz.string) (Fuzz.intRange 1 10) "The list should not have more elements than words on a clean string" <|
            \string width ->
                String.Extra.softBreak width string
                    |> List.length
                    |> Expect.atMost (String.words string |> List.length)
        ]


cleanTest : Test
cleanTest =
    describe "clean"
        [ fuzz Fuzz.string "The String.split result is the same as String.words" <|
            \string ->
                string
                    |> String.Extra.clean
                    |> String.split " "
                    |> Expect.equal (String.words string)
        , fuzz Fuzz.string "It trims the string on the left side" <|
            \string ->
                string
                    |> String.Extra.clean
                    |> String.startsWith " "
                    |> Expect.equal False
                    |> Expect.onFail "Did not trim the start of the string"
        , fuzz Fuzz.string "It trims the string on the right side" <|
            \string ->
                string
                    |> String.Extra.clean
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
                    |> String.Extra.insertAt sub at
                    |> String.contains sub
                    |> Expect.equal True
                    |> Expect.onFail "Could not find substitution string in result"
        , fuzz insertAtProducer "Resulting string has length as the sum of both arguments" <|
            \( sub, at, string ) ->
                String.Extra.insertAt sub at string
                    |> String.length
                    |> Expect.equal (String.length sub + String.length string)
        , fuzz insertAtProducer "Start of the string remains the same" <|
            \( sub, at, string ) ->
                String.Extra.insertAt sub at string
                    |> String.slice 0 at
                    |> Expect.equal (String.slice 0 at string)
        , fuzz insertAtProducer "End of the string remains the same" <|
            \( sub, at, string ) ->
                String.Extra.insertAt sub at string
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
                String.Extra.isBlank ""
                    |> Expect.equal True
                    |> Expect.onFail "Did not return true"
        , test "Returns false if the given string is not blank" <|
            \_ ->
                String.Extra.isBlank " Slartibartfast"
                    |> Expect.equal False
                    |> Expect.onFail "Did not return false"
        , fuzz whitespaceHeavyStringFuzzer "Oracle test: ensure no behaviour change between implementations" <|
            let
                trimBased str =
                    String.trim str == ""

                regexBased str =
                    Regex.contains (regexFromString "^\\s*$") str
            in
            \str ->
                trimBased str
                    |> Expect.equal (regexBased str)
        ]


whitespaceHeavyStringFuzzer : Fuzzer String
whitespaceHeavyStringFuzzer =
    Fuzz.list whitespaceHeavyCharFuzzer
        |> Fuzz.map String.fromList


whitespaceHeavyCharFuzzer : Fuzzer Char
whitespaceHeavyCharFuzzer =
    Fuzz.frequency
        [ ( 3, Fuzz.constant ' ' )
        , ( 3, Fuzz.constant '\n' )
        , ( 3, Fuzz.constant '\t' )
        , ( 3, Fuzz.constant '\u{000D}' ) -- \r
        , ( 1, Fuzz.char )
        ]


nonBlankTest : Test
nonBlankTest =
    describe "nonBlank"
        [ test "Returns Nothing if the given string is blank" <|
            \_ ->
                String.Extra.nonBlank ""
                    |> Expect.equal Nothing
        , test "Returns just the string if the given string is not blank" <|
            \_ ->
                String.Extra.nonBlank " Slartibartfast"
                    |> Expect.equal (Just " Slartibartfast")
        ]


surroundTest : Test
surroundTest =
    describe "surround"
        [ fuzz2 Fuzz.string Fuzz.string "It starts with the wrapping string" <|
            \string wrap ->
                string
                    |> String.Extra.surround wrap
                    |> String.startsWith wrap
                    |> Expect.equal True
                    |> Expect.onFail "Did not start with the wrapping string"
        , fuzz2 Fuzz.string Fuzz.string "It ends with the wrapping string" <|
            \string wrap ->
                string
                    |> String.Extra.surround wrap
                    |> String.endsWith wrap
                    |> Expect.equal True
                    |> Expect.onFail "Did not end with the wrapping string"
        , fuzz2 Fuzz.string Fuzz.string "It contains the original string" <|
            \string wrap ->
                string
                    |> String.Extra.surround wrap
                    |> String.contains string
                    |> Expect.equal True
                    |> Expect.onFail "Did not contain the string"
        , fuzz2 Fuzz.string Fuzz.string "It does not have anything else inside" <|
            \string wrap ->
                let
                    result =
                        String.length (String.Extra.surround wrap string)

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
                        String.Extra.countOccurrences needle haystack

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
                String.Extra.ellipsis howLong string
                    |> String.length
                    |> (\x -> howLong >= x)
                    |> Expect.equal True
                    |> Expect.onFail "Resulting string exceeds specified length"
        , fuzz2 (Fuzz.intRange 3 20) Fuzz.string "The resulting string contains three dots at the end if necessary" <|
            \howLong string ->
                let
                    result =
                        String.Extra.ellipsis howLong string
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
                        String.Extra.ellipsis howLong string

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
                String.Extra.unquote "\"Magrathea\""
                    |> Expect.equal "Magrathea"
        ]


wrapTest : Test
wrapTest =
    describe "wrap"
        [ fuzz2 (Fuzz.intRange 1 20) Fuzz.string "Wraps given string at the requested length" <|
            \howLong string ->
                String.Extra.wrap howLong string
                    |> String.split "\n"
                    |> List.map (\str -> String.length str <= howLong)
                    |> List.all ((==) True)
                    |> Expect.equal True
                    |> Expect.onFail "Given string was not wrapped at requested length"
        , test "Does not wrap string shorter than the requested length" <|
            \_ ->
                String.Extra.wrap 50 "Heart of Gold"
                    |> String.contains "\n"
                    |> Expect.equal False
                    |> Expect.onFail "Short string was wrapped"
        ]


pluralizeTest : Test
pluralizeTest =
    describe "pluralize"
        [ test "It uses the singular version when the count is one" <|
            \() ->
                String.Extra.pluralize "elf" "elves" 1
                    |> Expect.equal "1 elf"
        , test "It uses the plural version for > 1 count" <|
            \() ->
                String.Extra.pluralize "elf" "elves" 4
                    |> Expect.equal "4 elves"
        , test "It uses the plural version for 0 count" <|
            \() ->
                String.Extra.pluralize "elf" "elves" 0
                    |> Expect.equal "0 elves"
        ]


leftOfBackTest : Test
leftOfBackTest =
    test "leftOfBack" <|
        \() ->
            String.Extra.leftOfBack "_" "This_is_a_test_string"
                |> Expect.equal "This_is_a_test"


leftOfTest : Test
leftOfTest =
    describe "leftOf"
        [ test "basic contains multiple" <|
            \() ->
                String.Extra.leftOf "___" "This___is_a___test___string"
                    |> Expect.equal "This"
        , test "not contains" <|
            \() ->
                String.Extra.leftOf "-" "This_is_a_test_string"
                    |> Expect.equal ""
        , test "newlines" <|
            \() ->
                String.Extra.leftOf "_" "This\ntest_string_foo"
                    |> Expect.equal "This\ntest"
        ]


rightOfBackTest : Test
rightOfBackTest =
    describe "rightOfBack"
        [ test "basic contains multiple" <|
            \() ->
                String.Extra.rightOfBack "___" "This___is_a___test___string"
                    |> Expect.equal "string"
        , test "not contains" <|
            \() ->
                String.Extra.rightOfBack "-" "This_is_a_test_string"
                    |> Expect.equal ""
        , test "newlines" <|
            \() ->
                String.Extra.rightOfBack "_" "This_is_a\ntest_string"
                    |> Expect.equal "string"
        ]


rightOfTest : Test
rightOfTest =
    describe "rightOf"
        [ test "basic contains multiple" <|
            \() ->
                String.Extra.rightOf "___" "This___is_a___test___string"
                    |> Expect.equal "is_a___test___string"
        , test "not contains" <|
            \() ->
                String.Extra.rightOf "-" "This_is_a_test_string"
                    |> Expect.equal ""
        , test "newlines" <|
            \() ->
                String.Extra.rightOf "_" "This_is_a\ntest_string"
                    |> Expect.equal "is_a\ntest_string"
        ]


underscoredTest : Test
underscoredTest =
    underscoredDasherizeTestHelper String.Extra.underscored "_" "underscored"


dasherizeTest : Test
dasherizeTest =
    underscoredDasherizeTestHelper String.Extra.dasherize "-" "dasherize"


underscoredDasherizeTestHelper : (String -> String) -> String -> String -> Test
underscoredDasherizeTestHelper testFn testChar testLabel =
    describe testLabel
        [ fuzz string "It is a lowercased string" <|
            \s ->
                testFn s
                    |> String.toLower
                    |> Expect.equal (testFn s)
        , fuzz string "It has no spaces in the resulting string" <|
            \s ->
                let
                    whiteSpaceChecker =
                        List.any Char.Extra.isSpace
                in
                testFn (String.toLower s)
                    |> String.toList
                    |> whiteSpaceChecker
                    |> Expect.equal False
        , fuzz string "It has no consecutive target characters in the resulting string" <|
            \s ->
                let
                    consecutiveDashesChecker =
                        Regex.contains <| consecutiveCharacterRegex testChar
                in
                testFn (String.toLower s)
                    |> consecutiveDashesChecker
                    |> Expect.equal False
        ]


consecutiveCharacterRegex : String -> Regex
consecutiveCharacterRegex charStr =
    Regex.fromString (charStr ++ "{2,}")
        |> Maybe.withDefault Regex.never


regexFromString : String -> Regex
regexFromString str =
    Regex.fromString str
        |> Maybe.withDefault Regex.never
