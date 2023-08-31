module String.TestData exposing
    ( randomStrings
    , randomStringsWithChars
    )

import Fuzz exposing (Fuzzer)


randomStrings : Fuzzer String
randomStrings =
    randomStringsWithChars []


randomStringsWithChars : List Char -> Fuzzer String
randomStringsWithChars chars =
    Fuzz.listOfLengthBetween 0
        10
        (case chars of
            [] ->
                Fuzz.map Char.fromCode (Fuzz.oneOf [ Fuzz.intRange 97 122, Fuzz.intRange 65 90 ])

            _ ->
                Fuzz.oneOf [ Fuzz.map Char.fromCode (Fuzz.intRange 97 122), Fuzz.map Char.fromCode (Fuzz.intRange 65 90), Fuzz.oneOfValues chars ]
        )
        |> Fuzz.map String.fromList
